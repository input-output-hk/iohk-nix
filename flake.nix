{
  description = "IOHK nix lib, packages and overlays";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    let
      lib = import ./lib nixpkgs.lib;

      overlays = {
        crypto = import ./overlays/crypto;
        haskell-nix-extra = import ./overlays/haskell-nix-extra;
        cardano-lib = (final: prev: {
          cardanoLib = final.callPackage ./cardano-lib {};
        });
        utils = import ./overlays/utils;
      };

      cabal-wrapper = ./pkgs/cabal-wrapper.nix;

      # Where is this used? Can we rename this?
      # Having "checks" that are not flake checks breaks
      # `nix flake show` and `nix flake check`.
      checks = {
        hlint = ./tests/hlint.nix;
        shell = ./tests/shellcheck.nix;
        stylish-haskell = ./tests/stylish-haskell.nix;
      };

      utils = {
        cabal-project = ./ci/cabal-project-regenerate;
      };

      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
    in {
        inherit lib overlays cabal-wrapper checks utils;
      } // flake-utils.lib.eachSystem supportedSystems (system:
        let 
          pkgs = import nixpkgs {
            overlays = [haskellNix.overlay] ++ __attrValues overlays;
            inherit system;
            inherit (haskellNix) config;
          };
        in {
          # These are for checking IOG projects build in an environment
          # without haskell packages built by haskell.nix.
          #
          # Usage:
          #
          # nix develop github:input-output-hk/iohk-nix#ghc924 --no-write-lock-file -c cabal build
          #
          devShells = __mapAttrs (compiler-nix-name: compiler:
            pkgs.mkShell {
              buildInputs = [
                compiler
                pkgs.haskell-nix.cabal-install.${compiler-nix-name}
                pkgs.pkgconfig
              ] ++ map pkgs.lib.getDev (with pkgs; [ libsodium-vrf secp256k1 R zlib openssl ] ++ pkgs.lib.optional pkgs.stdenv.hostPlatform.isLinux systemd);
            }
          ) (builtins.removeAttrs pkgs.haskell-nix.compiler
              # Exclude old versions of GHC to speed up `nix flake check`
              [ "ghc844"
                "ghc861" "ghc862" "ghc863" "ghc864"
                "ghc881" "ghc882" "ghc883"
                "ghc8101" "ghc8102" "ghc8103" "ghc8104" "ghc8105" "ghc8106" "ghc810420210212"
                "ghc901"
                "ghc921" "ghc922" "ghc923"]);
      });
}
