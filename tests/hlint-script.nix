{ pkgs }:

with pkgs;
let
  deps = [ hlint ];
in
  writeScript "hlint.sh" ''
    #!${stdenv.shell}
    set +e
    export PATH=${lib.makeBinPath deps}
    hlint "$1"
    EXIT_CODE=$?
    if [[ $EXIT_CODE != 0 ]]
    then
      echo '====================================================================='
      echo 'Note: to ignore a particular hint (e.g. "Reduce duplication"), write'
      echo 'this in the source file:'
      echo '{-# ANN module ("HLint: ignore Reduce duplication" :: Text) #-}'
      echo 'You can also apply it just to a particular function, which is better:'
      echo '{-# ANN funcName ("HLint: ignore Reduce duplication" :: Text) #-}'
      exit $EXIT_CODE
    fi
  ''
