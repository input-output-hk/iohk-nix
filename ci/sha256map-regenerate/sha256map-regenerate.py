#!/usr/bin/env python3
#
# This script is intended to be run as
#
#  python3 sha256map-regenerate.py
#
# And it will iterate over all git references in the stack.yaml file
# (TODO: or cabal.project) and generate a set of sha256 value for them
# in the nix/sha256map.nix file, that can then be imported as such:
# haskell-nix.stackProject' { sha256map = import ./sha256map.nix; }
# This then allows us to build this project with haskell.nix in a
# restricted nix setup where network access is only permitted if the
# sha256 of the download is known beforehand.

import re
import subprocess
import json
import sys

# Read stack.yaml from stin
project = sys.stdin.read()

# match looks like this:

#- git: https://github.com/input-output-hk/cardano-base
#  commit: e8a48cf0500b03c744c7fc6f2fedb86e8bdbe055

pattern = r'\- git: (?P<loc>[^ \n]+).*\n' \
        + r'(?P<pad> .*)commit: (?P<tag>[^ \n]+).*\n'

def sha256entry(match):
  dict = match.groupdict()
  prefetchJSON = subprocess.run(
    ["nix-prefetch-git", "--fetch-submodules", "--quiet", dict['loc'], dict['tag']],
    capture_output=True, check=True).stdout
  sha256 = json.loads(prefetchJSON)["sha256"]
  return '"{loc}"."{tag}" = "{sha256}";'.format(**{**dict, **{"sha256": sha256}})

# Write sha256map to stdout
print("{")
for x in sorted([sha256entry(match) for match in re.finditer(pattern, project)]):
  print(" ", x)
print("}")
