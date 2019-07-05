#!/usr/bin/env nix-shell
#!nix-shell -i bash -p nix-prefetch-git coreutils jq

set -euo pipefail

NIX_DIR=`dirname $0`

echo "+++ :pushpin: iohk-nix-src.json"
nix-prefetch-git \
    --quiet \
    --rev "$BUILDKITE_COMMIT" \
    https://github.com/input-output-hk/iohk-nix \
    | tee iohk-nix-src.json | jq .

echo "--- Upload artifact"
buildkite-agent artifact upload iohk-nix-src.json --job "$BUILDKITE_JOB_ID"
