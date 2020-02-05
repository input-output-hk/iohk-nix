#!/usr/bin/env bash

set -euo pipefail

exec $(nix-build `dirname $0` -A commonLib.nix-tools.regenerateStackPackages --no-out-link)
