{ stdenv
, lib
, runtimeShell
, writeScript
, runCommand
, python37
, python3Packages
, coreutils
, diffutils
, nix-prefetch-git
}:
let
  python = python37;
  cabalProjectRegenerate = runCommand "cabal-project-regenerate" {} ''
    mkdir -p $out/bin
    cp ${./cabal-project-regenerate.py} $out/cabal-project-regenerate.py
    cat << EOF > $out/bin/cabal-project-regenerate
    PATH=$PATH:${nix-prefetch-git}/bin
    ${python}/bin/python $out/cabal-project-regenerate.py
    EOF
    chmod +x $out/bin/cabal-project-regenerate
  '';
  checkDeps = [
    coreutils
    diffutils
    cabalProjectRegenerate
    nix-prefetch-git
  ];
  checkCabalProject = writeScript "check-cabal-project" ''
    #!${runtimeShell}
    PATH=${lib.makeBinPath checkDeps}
    set -euo pipefail
    cp ./cabal.project ./cabal.project.old
    cabal-project-regenerate
    set +e
    diff -u cabal.project.old cabal.project > cabal.project.diff
    if [ $? != 0 ]
    then
      echo "Changes required to cabal.project"
      cat cabal.project.diff
      exit 1
    else
      echo "No changes needed to cabal.project"
      exit 0
    fi
  '';
in { inherit cabalProjectRegenerate checkCabalProject; }
