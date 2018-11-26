{ pkgs }:

with pkgs;
let
  deps = [ shellcheck findutils ];
in
  writeScript "shellcheck.sh" ''
    #!${stdenv.shell}
    set +e
    export PATH=${lib.makeBinPath deps}
    EXIT_STATUS=0
    cd "$1"
    while IFS= read -r -d ''' i
    do
      if shellcheck -x -e 1008 -e 2148 "$i"
      then
        echo "$i [ PASSED ]"
      else
        echo "$i [ FAILED ]"
        EXIT_STATUS=$(($EXIT_STATUS+1))
      fi
    done <  <(find -name '*.sh' -print0)
    echo Total Failed Files: $EXIT_STATUS
    exit "$EXIT_STATUS"
  ''
