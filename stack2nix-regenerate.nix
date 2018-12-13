{ stdenv
, lib
, writeScript
, nixStable
, coreutils
, git
, findutils
, cabal2nix
, glibcLocales
, stack
, cabal-install
, stack2nix
, path
# Set this to a recent enough date that include any package versions that you need from hackage.
# Any timestamp works.
, hackageSnapshot
}:

let
  deps = [ nixStable coreutils git findutils cabal2nix glibcLocales stack cabal-install stack2nix ];
in
  writeScript "generate" ''
    #!${stdenv.shell}
    #
    # Haskell package set regeneration script.
    #
    # stack2nix will transform the stack.yaml file into something
    # nix can understand.
    # 
    # Takes one argument which is the file to write the package set into.

    set -euo pipefail
    export PATH=${lib.makeBinPath deps}
    export HOME=$(pwd)
    export NIX_PATH=nixpkgs=${path}

    echo "Using hackage snapshot from ${hackageSnapshot}"

    # Ensure that nix 1.11 (used by cabal2nix) works in multi-user mode.
    if [ ! -w /nix/store ]; then
        export NIX_REMOTE=''${NIX_REMOTE:-daemon}
    fi

    dest=$1
    function cleanup {
      rm -f "$dest.new"
    }
    trap cleanup EXIT

    # Generate package set
    stack2nix --platform x86_64-linux --hackage-snapshot "${hackageSnapshot}" -j8 --test --bench --no-indent ../. > "$dest.new"
    mv "$dest.new" "$dest"

    echo "Wrote $dest"
  ''
