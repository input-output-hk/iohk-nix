{ pkgs }:
let

  overrideWith = import ./fetch-tarball-with-override.nix;

in
  import (overrideWith "haskell" ./pins/haskell-nix.json) {
    inherit pkgs;
    # hackageSourceJSON = ./pins/hackage-nix.json;
    # stackageSourceJSON = ./pins/stackage-nix.json;
  }
