package dql

import cats.Order
import cats.instances.order.catsKernelOrderingForOrder
import cats.effect.*
import std.*
import cats.syntax.all.*
import spire.algebra.Ring
import spire.syntax.ring.*

trait Environment[State, Action, Reward]:

  /**
    * Given the current state, what are the legal actions the agent can take?
    */
  def possibleActions(currentState: State): List[Action]

  /**
  * Given the current state and the action chosen by the agent,
  * what state does the agent move into and what reward does it get?
  *
  * Things to note:
  * - The reward might be positive, negative or zero.
  * - The next state might be the same as the current state.
  * - Both the state transition function and the reward function may be stochastic,
  *   meaning they follow some probability distribution and do not always
  *   give the same output for a given input.
  */
  def step( 
    currentState: State,
    actionTaken: Action,
  ): IO[(State,Reward)]

  /**
    * Is the given state terminal or not?
    * For continuous (non-episodic) problems, this will always be false.
    */
  def isTerminal(state: State): Boolean

case class ActionResult[State,Reward](reward: Reward, nextState: State)

trait AgentBehavior[AgentData, State, Action, Reward]:
  def chooseAction(
    agentData: AgentData,
    state: State,
    validActions: List[Action]
  ): IO[(Action, ActionResult[State,Reward] => AgentData)]

trait StateConversion[EnvState, AgentState]:

  /**
    * Convert from the "true", complete state as known by the environment,
    * into a simplified state that we give to the agent.
    *
    * This is a chance to do things:
    *
    * 1. If the problem includes any constraints that say the agent should have incomplete
    *    knowledge of the environment, we can encode that here.
    *
    * 2. We can discard some information in order to reduce the agent's state space,
    *    e.g. by bucketing a large number of environment states into a single agent state.
    */
  def convertState(envState: EnvState): AgentState


case class QLearning[State, Action, Reward](
    alpha: Reward, // step size, 0.0 ≦ α ≦ 1.0, controls how much the agent updates its action-value function Q(s, a)
    gamma: Reward, // discount rate, 0.0 ≦ γ ≦ 1.0, controls how much the one-step backup affects Q(s, a)
    epsilon: Double, // 0.0 ≦ ε ≦ 1.0, probability of choosing a random action
    Q: Map[State, Map[Action, Reward]] // the estimated action-value function Q(s, a)
)

/**
  * Runner
  * 
  * start with initial agent data and state
  * at every timestep:
  *   1. ask agent to choose an action
  *   2. tell the environment, which will return the new state and a reward
  *   3. tell these to the agent, which will return an improved version of itself
  *   4. update UI (or other side effect)
  */

object QLearning:
  def step[AgentData, State, Action, Reward](
      agentData: AgentData, 
      currentState: State
    )(
      using env: Environment[State, Action, Reward],
      ab: AgentBehavior[AgentData, State, Action, Reward]
    ): IO[(AgentData, State)] =
    for {
      (nextAction, updateAgent) <- ab.chooseAction(agentData,currentState, List.empty)
      (nextState, reward) <- env.step(currentState,nextAction)
      nextAgentData <- IO(updateAgent(ActionResult(reward,nextState)))
    } yield (nextAgentData, nextState)

  def agentBehavior[State,Action,Reward](using Random[IO], Order[Reward], Ring[Reward])
    : IO[AgentBehavior[QLearning[State,Action,Reward],State,Action,Reward]] = IO {
        new AgentBehavior[QLearning[State,Action,Reward],State,Action,Reward] {

          def chooseAction(agentData: QLearning[State, Action, Reward], state: State, validActions: List[Action])
            : IO[(Action, ActionResult[State, Reward] => QLearning[State, Action, Reward])] = for {

              // get Q(s, {a]}), or initialize arbitrarily to 0 for all actions if not initialized yet
              actionValues <- IO(agentData.Q).map(_.getOrElse(state, validActions.map(_ -> Ring[Reward].zero).toMap))

              // choose the next action
              (chosenAction, currentActionValue) <- epsilonGreedy(actionValues,agentData.epsilon)

              // learn!
              updateStateActionValue <- IO{ 
                (actionResult: ActionResult[State,Reward]) => {
                  val nextStateActionValues = agentData.Q.getOrElse(actionResult.nextState, validActions.map(_ -> Ring[Reward].zero).toMap)
                  val maxNextStateActionValue = nextStateActionValues.maxBy(_._2)

                  // Q(s_t, a_t) <- Q(s_t, a_t) + α (r_t+1 + γ max_a Q(s_t+1, a) - Q(s_t, a_t))
                  val updatedActionValue =
                    currentActionValue + agentData.alpha * (actionResult.reward + agentData.gamma* maxNextStateActionValue._2 - currentActionValue)

                  val updatedActionValues = actionValues + (chosenAction -> updatedActionValue)
                  val updatedQ            = agentData.Q + (state         -> updatedActionValues)

                  agentData.copy(Q = updatedQ)
                }
              }
            } yield (chosenAction, updateStateActionValue)

          /*
          ε-greedy: choose one of the actions with the highest value most of the time (i.e. exploit)
          but choose an action randomly some of the time (i.e. explore)
          */
          private def epsilonGreedy(actionValues: Map[Action,Reward], epsilon: Double): IO[(Action, Reward)] = {
            Random[IO].nextDouble.map(_ < epsilon).ifM(
              Random[IO].shuffleList(actionValues.toList).map(_.head),
                IO(actionValues.toList.sortBy(_._2).reverse).flatMap {
                  sorted => for {
                    maxValue <- IO(sorted.head._2)
                    taken <- IO(sorted.takeWhile(_._2 == maxValue))
                    shuffled <- Random[IO].shuffleList(taken)
                  } yield shuffled.head
                }
            )
          }
        }
    }