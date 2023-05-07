{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    haskell-flake.url = "github:srid/haskell-flake";
    devenv = {
      url = "github:cachix/devenv";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nix.follows = "blank";
      };
    };
    blank.url = "github:divnix/blank";
  };
  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.devenv.flakeModule
      ];

      perSystem = { config, self', pkgs, lib, ... }: let
        inherit (config.haskellProjects.default.outputs) finalPackages;
      in {
        haskellProjects.default = {
          autoWire = false;
          devShell.enable = false;
        };
        packages = rec {
          inherit (finalPackages) hercules;
          default = hercules;
        };
        devenv.shells.default = let
          rawShell = finalPackages.shellFor {
            packages = lib.const [ finalPackages.hercules ];
            nativeBuildInputs = [
              pkgs.cabal-install
              pkgs.haskell-language-server
            ];
            withHoogle = true;
          };
        in {
          env.DEVENV_ROOT = lib.mkForce "/tmp";
          packages = rawShell.nativeBuildInputs;
          processes = {
            hercules.exec = "cabal run";
          };
          scripts = {
            docs.exec = ''
              echo Hoogle listening on http://127.0.0.1:30001
              hoogle server --local --port 30001
            '';
            new.exec = ''
              [[ $# -ne 1 ]] && echo 'Usage: new <module name>' && exit 1
              moduleName="$1"
              modulePath="src/$(tr . / <<<"$moduleName").hs"
              moduleDir="$(dirname "$modulePath")"
              echo "Creating module $moduleName in $modulePath"
              mkdir -p "$moduleDir"
              cat > "$modulePath" <<EOF
              module $moduleName where

              import Universum
              EOF
            '';
          };
          # domen pls
          containers = lib.mkForce {};
        };
      };
    };
}
