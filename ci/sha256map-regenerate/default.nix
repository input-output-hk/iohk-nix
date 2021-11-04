{ writeScript
, runtimeShell
, lib
, python37
, nix-prefetch-git
}:
writeScript "sha256map-regenerate.sh" ''
  #!${runtimeShell}
  export PATH="${lib.makeBinPath [ nix-prefetch-git python37 ]}"
  ${python37}/bin/python ${./sha256map-regenerate.py}
''
