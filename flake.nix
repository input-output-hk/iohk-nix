{
  description = "IOHK nix lib, packages and overlays";

  inputs.nixpkgs.url = "github:nixos/nixpkgs?ref=release-22.11";

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


    # we can use this, to get a coherent picture of the sources for
    # the various libraries.  The following command will produce a
    # JSON output, that contains each of our libs, with their respective
    # versions.
    #
    #    nix eval --json .#lib-srcs
    #
    lib-srcs = {
      secp256k1 = pkgs.secp256k1.src.url;
      sodium    = pkgs.libsodium-vrf.src.url;
      blst      = pkgs.libblst.src.url;
    };
  };
}
