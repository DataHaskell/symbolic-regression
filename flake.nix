{
  description = "DataFrame symbolic regression";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];

      perSystem =
        {
          config,
          pkgs,
          system,
          ...
        }:
        let
          pkgs' = import inputs.nixpkgs {
            inherit system;
            config.allowBroken = true;
          };

          haskellPackages = pkgs'.haskellPackages.extend (
            final: prev: {
              # Use the correct hash that Nix gave us
              random = pkgs'.haskell.lib.dontCheck (
                prev.callHackageDirect {
                  pkg = "random";
                  ver = "1.3.0";
                  sha256 = "sha256-AzXPz8oe+9lAS60nRNFiRTEDrFDB0FPLtbvJ9PB7brM=";
                } { }
              );

              srtree = pkgs'.haskell.lib.dontCheck (
                prev.callHackageDirect {
                  pkg = "srtree";
                  ver = "2.0.1.6";
                  sha256 = "sha256-D561wnKoFRr/HSviacsbtF5VnJHehG73LOmWM4TxlNs=";
                } { }
              );

              dataframe = pkgs'.haskell.lib.dontCheck (
                prev.callHackageDirect {
                  pkg = "dataframe";
                  ver = "0.4.1.0";
                  sha256 = "sha256-u4FrhD+oOnQiGazq/TWuL0PzUvLKKAkz679tZSkiMaY=";
                } { }
              );

              # Jailbreak packages that depend on old random
              time-compat = pkgs'.haskell.lib.doJailbreak prev.time-compat;
              splitmix = pkgs'.haskell.lib.doJailbreak prev.splitmix;
            }
          );

        in
        {
          packages = {
            default = config.packages.symbolic-regression;
            symbolic-regression = haskellPackages.callCabal2nix "symbolic-regression" ./. { };
          };

          devShells.default = haskellPackages.shellFor {
            packages = p: [ config.packages.symbolic-regression ];
            buildInputs = with pkgs'; [
              cabal-install
              haskell-language-server
              git
              fourmolu
            ];
          };
        };
    };
}
