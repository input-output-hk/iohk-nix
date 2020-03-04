#!/usr/local/python3
#
# This script is intended to be run as
#
#  python3 update-cabal-project.py
#
# And it will iterate over all git references in the cabal.project file
# and add a --sha256 entry to them.  This then allows us to build this
# project with haskell.nix in a restricted nix setup where network access
# is only permitted if the sha256 of the download is known beforehand.

import re
import subprocess
import json
from os import path

globalCache = {}

if not path.exists("cabal.project"):
    print("ERROR: No cabal.project exists in current directory")
    exit(1)

# TODO: copy to temporary location and replace if no errors
buffer = open("cabal.project").read()

# match looks like this:

# source-repository-package
#  type: git
#  location: https://github.com/input-output-hk/iohk-monitoring-framework
#  tag: d5eb7ee92f0974aed5ac4a57a9bf5697ec065b7d
#  subdir: iohk-monitoring

# subdir is optional, so is a --sha256 comment

pattern = r'source-repository-package.*\n' \
        + r'(?P<pad> .*)type: git.*\n' \
        + r'\1location: (?P<loc>[^ \n]+).*\n' \
        + r'\1tag: (?P<tag>[^ \n]+).*\n' \
        + r'(\1--sha256:.*\n){0,1}' \
        + r'(?P<subdir>\1subdir: [^ \n]+\n){0,1}' \
        + r'(\1--sha256:.*\n){0,1}'
# ( .*--sha256:.*\n){0,1}"""

def repl(match):
  dict = match.groupdict()
  if not dict["subdir"]:
    dict["subdir"] = ''
  if not (dict['loc'], dict['tag']) in globalCache:
    prefetchJSON = subprocess.run(
      ["nix-prefetch-git", "--fetch-submodules", "--quiet", dict['loc'], dict['tag']],
      capture_output=True, check=True).stdout
    globalCache[(dict['loc'], dict['tag'])] = json.loads(prefetchJSON)["sha256"]
  sha256 = globalCache[(dict['loc'], dict['tag'])]
  return """source-repository-package
{pad}type: git
{pad}location: {loc}
{pad}tag: {tag}
{pad}--sha256: {sha256}
{subdir}""".format(**{**dict, **{"sha256": sha256}})

f = open("cabal.project",'w')
f.write(re.sub(pattern, repl, buffer, flags = re.I + re.M))
f.close()
