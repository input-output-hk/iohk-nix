#!/usr/local/python3
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
from os import path

if not path.exists("stack.yaml"):
    print("ERROR: No stack.yaml exists in current directory")
    exit(1)

project = open("stack.yaml").read()

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
  return """
  "{loc}"."{tag}" = "{sha256}";
""".format(**{**dict, **{"sha256": sha256}})

f = open("nix/sha256map.nix",'w')

f.write("{")
for match in re.finditer(pattern, project):
  f.write(sha256entry(match))
f.write("}\n")

f.close()
