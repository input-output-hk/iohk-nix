{ pkgs }:
let

  overrideWith = import ./fetch-tarball-with-override.nix;

in rec {
  # all packages from hackage as nix expressions
  hackage = import  (overrideWith "hackage" ./pins/hackage-nix.json);

  # a different haskell infrastructure
  haskell = import (overrideWith "haskell" ./pins/haskell-nix.json) hackage;

  # the set of all stackage snapshots
  stackage = import (overrideWith "stackage" ./pins/stackage-nix.json);
}
