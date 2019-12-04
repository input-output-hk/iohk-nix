#!/usr/bin/env bash

set -euo pipefail

exec $(nix-build `dirname $0` -A commonLib.nix-tools.regeneratePackages --no-out-link)
