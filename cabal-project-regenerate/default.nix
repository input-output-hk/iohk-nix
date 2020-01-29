{ runCommand, python3, python3Packages }:
let
  python = python3;
in runCommand "cabal-project-regenerate" {} ''
  mkdir -p $out
  cp ${./cabal-project-regenerate.py} $out/cabal-project-regenerate.py
  cat << EOF > $out/cabal-project-regenerate
  ${python}/bin/python $out/cabal-project-regenerate.py
  EOF
  chmod +x $out/cabal-project-regenerate
''
