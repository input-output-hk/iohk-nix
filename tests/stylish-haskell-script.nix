{ pkgs }:

with pkgs;
let
  deps = [ haskellPackages.stylish-haskell findutils diffutils glibcLocales coreutils ];
in
  writeScript "stylish-haskell.sh" ''
    #!${stdenv.shell}
    set -euo pipefail
    export PATH=${lib.makeBinPath deps}
    export HOME=/dev/null
    SRC="$1"
    cp -aL "$SRC" stylish
    chmod -R +w stylish
    cd stylish
    find . -type f -name "*.hs" -not -name 'HLint.hs' -exec stylish-haskell -i {} \;
    cd ..
    set +e
    diff --brief --recursive "$SRC" stylish
    EXIT_CODE=$?
    if [[ $EXIT_CODE != 0 ]]
    then
      diff -ur "$SRC" stylish
      echo "*** Stylish-haskell found changes that need addressed first"
      echo "*** Please run \`nix-shell -A fixStylishHaskell\` and commit changes"
      echo "*** or apply the generated diff if you don't have nix."
      exit $EXIT_CODE
    fi
  ''
