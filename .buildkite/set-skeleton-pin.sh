#!/usr/bin/env bash

set -euo pipefail

echo "--- :pushpin: Get pin from main build"
buildkite-agent artifact download iohk-nix-src.json skeleton/nix/
