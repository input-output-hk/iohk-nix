{ runCommand, python37, python3Packages }:
let
  python = python37;
in runCommand "cabal-project-regenerate" {} ''
  mkdir -p $out/bin
  cp ${./cabal-project-regenerate.py} $out/cabal-project-regenerate.py
  cat << EOF > $out/bin/cabal-project-regenerate
  ${python}/bin/python $out/cabal-project-regenerate.py
  EOF
  chmod +x $out/bin/cabal-project-regenerate
''
