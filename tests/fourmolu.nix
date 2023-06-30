{ runCommand, fourmolu, src, lib }:

let
  # Remove everything except Haskell sources, fourmolu configuration, and cabal files.
  src' = lib.cleanSourceWith {
    inherit src;

    filter = with lib; name: type:
      let baseName = baseNameOf (toString name); in (
        (type == "regular" && hasSuffix ".hs" baseName) ||
        (type == "regular" && hasSuffix ".yaml" baseName) ||
        (type == "regular" && hasSuffix ".cabal" baseName) ||
        (type == "directory")
      );
  };
in

runCommand "fourmolu-check" { buildInputs = [ fourmolu ]; } ''
  set +e
  cd ${src'}
  fourmolu --mode check .
  EXIT_CODE=$?

  echo $EXIT_CODE > $out

  exit $EXIT_CODE
''
