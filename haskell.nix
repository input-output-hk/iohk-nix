{ haskellNixJsonOverride } :
{ pkgs }:
let

  overrideWith = import ./fetch-tarball-with-override.nix;
  haskellNixJson = if (haskellNixJsonOverride != "")
    then haskellNixJsonOverride
    else ./pins/haskell-nix.json;
  path = overrideWith "haskell" haskellNixJson;
in
  # To update this pin, run ./pins/update-defaults.sh
  (import path {
    inherit pkgs;
    # hackageSourceJSON = ./pins/hackage-nix.json;
    # stackageSourceJSON = ./pins/stackage-nix.json;
  }) // { inherit path; }
