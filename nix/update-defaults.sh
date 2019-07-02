#!/bin/sh

set -euo pipefail

cd `dirname $0`/..

NIV=`nix-build --no-out-link -E '(import ./. {}).niv'`/bin/niv

$NIV init

for p in nixpkgs \
         nixpkgs-19_03 \
         nixpkgs-unstable \
         haskell \
         gitignore \
         niv \

do
    $NIV update $p
done