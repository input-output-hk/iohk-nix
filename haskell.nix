{ pkgs, hackageIndexState ? null }:
let

  overrideWith = import ./fetch-tarball-with-override.nix;

in
  # To update this pin, run ./pins/update-defaults.sh
  import (overrideWith "haskell" ./pins/haskell-nix.json) {
    inherit pkgs hackageIndexState;
    # hackageSourceJSON = ./pins/hackage-nix.json;
    # stackageSourceJSON = ./pins/stackage-nix.json;
  }
