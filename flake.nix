{
  description = "dql-from-scratch build environment";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: 
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux.pkgs;
    in {
      devShells.x86_64-linux.default = pkgs.mkShell {
        name = "dql-from-scratch";
        shellHook = ''
          echo "Welcome to: $name"
          PS1="($name/$(git branch --show-current 2>/dev/null)) $PS1";
        '';
        buildInputs = with pkgs; [
          bashInteractive 
          scala-cli 
        ];
      };
    };
}
