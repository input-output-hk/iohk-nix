#!/usr/bin/env nix-shell
#!nix-shell -i bash -p nix-prefetch-git

set -euo pipefail

PINS_DIR=`dirname $0`

nix-prefetch-git https://github.com/input-output-hk/nixpkgs \
                 --rev refs/heads/iohk-18.09 \
                 > $PINS_DIR/default-nixpkgs-src.json

nix-prefetch-git https://github.com/input-output-hk/hackage.nix \
                 > $PINS_DIR/hackage-nix.json

nix-prefetch-git https://github.com/input-output-hk/haskell.nix \
                 > $PINS_DIR/haskell-nix.json

nix-prefetch-git https://github.com/input-output-hk/stackage.nix \
                 > $PINS_DIR/stackage-nix.json