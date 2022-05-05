{ runCommand
, runtimeShell
, cabal-install
}:
runCommand "cabal" { inherit (cabal-install) meta; } ''
  mkdir -p $out/bin
  cat << EOF > $out/bin/cabal
  #!${runtimeShell}
  set -euo pipefail

  find_up() {
    while [[ \$PWD != / ]] ; do
      if [[ -e "\$1" ]]; then
        echo "\$PWD"
        return
      fi
      cd ..
    done
  }

  toplevel=\$(find_up "cabal.project")

  if [[ -n "\$toplevel" ]]; then
    cabal_project="\$toplevel/cabal.project"
    nix_cabal_project=\$toplevel/.nix-cabal.project
    extra_cabal_opts=("--project-file=\$nix_cabal_project")
    awk '
      # Add comment with explanation of file
      BEGIN {
        print "-- Generated from '"\$cabal_project"' by the wrapper script"
        print "-- ${placeholder "out"}/cabal"
        print "-- Add this file to your .gitignore\n"
      }

      # Matches all section starts
      /^[^ ]/ {
        # Remember the section name (they can span multiple lines)
        section = \$0
      }
      # Matches every line
      // {
        # Only print the line if it is not in the section we want to omit
        if (section != "source-repository-package")
          print \$0
      }
    ' "\$cabal_project" > "\$nix_cabal_project"
  else
    extra_cabal_opts=()
  fi

  cabal=${placeholder "out"}/bin/.cabal
  >&2 echo "\$cabal \''${extra_cabal_opts[@]} \$@"
  exec "\$cabal" "\''${extra_cabal_opts[@]}" "\$@"
  EOF
  cp -rn ${cabal-install}/* $out/
  cp ${cabal-install}/bin/cabal $out/bin/.cabal
  chmod +x $out/bin/*
''
