{
  description = "IOHK nix lib, packages and overlays";

  outputs = { self, nixpkgs }: rec {

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

    checks = {
      hlint = ./tests/hlint.nix;
      shell = ./tests/shellcheck.nix;
      stylish-haskell = ./tests/stylish-haskell.nix;
    };

    utils = {
      cabal-project = ./ci/cabal-project-regenerate;
      ciJobsAggregates = ./ci/aggregates.nix;
    };

    pkgs = import nixpkgs { system = "x86_64-linux"; overlays = builtins.attrValues overlays; };

  };
}
