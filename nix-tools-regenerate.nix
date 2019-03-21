# A script for generating the nix haskell package set based on stackage,
# using the common convention for repo layout.

{ lib, stdenv, path, writeScript, nix-tools, coreutils, findutils, gawk
, nix, nix-prefetch-scripts }:

let
  deps = [ nix-tools coreutils gawk nix nix-prefetch-scripts findutils ];

in
  writeScript "nix-tools-regenerate" ''
    #!${stdenv.shell}
    #
    # Haskell package set regeneration script.
    #
    # stack-to-nix will transform the stack.yaml file into something
    # nix can understand.
    #

    set -euo pipefail
    # See https://github.com/NixOS/nixpkgs/pull/47676 for why we add /usr/bin to
    # the PATH on darwin. The security-tool in nixpkgs is broken on macOS Mojave.
    export PATH=${(lib.makeBinPath deps) + lib.optionalString stdenv.isDarwin ":/usr/bin"}
    export NIX_PATH=nixpkgs=${path}

    tmp_dest=".stack-to-nix.tmp"
    mkdir -p "$tmp_dest"

    function cleanup {
      rm -rf "$tmp_dest"
    }
    trap cleanup EXIT

    stack-to-nix --output "$tmp_dest/.stack.nix"
    (
      cd $tmp_dest/.stack.nix
      mv pkgs.nix default.nix
    )

    generated_files="$(cd "$tmp_dest"; find .stack.nix)"

    cp -rlf $tmp_dest/.stack.nix nix/

    echo "Generated files in nix directory:"
    echo "$generated_files"
  ''
