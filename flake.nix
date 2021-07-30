{
  description = "Game 1";

  nixConfig.bash-prompt = "❄ Game 1 [$PWD]\nλ ";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
  };

  outputs = inputs@{ nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [];
      };
    in {
      devShell.${system} = import ./shell.nix pkgs;

      defaultPackage.${system} = import ./src pkgs;
    };
}
