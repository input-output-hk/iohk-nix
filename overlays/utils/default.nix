final: prev: let
  inherit (final) system git nixFlakes cabal writeShellScriptBin;
in {
  nixWrapped = writeShellScriptBin "nix" ''
    find_up() {
      while [[ $PWD != / ]] ; do
        if [[ -e "$1" ]]; then
          echo "$PWD"
          return
        fi
        if [[ -e "nix/$1" ]]; then
          echo "$PWD/nix"
          return
        fi
        cd ..
      done
    }
    if [[ "$@" == *"flake"*" "*"show"* ]] || [[ "$@" == *"flake"*" "*"check"* ]]; then
      >&2 echo 'Temporary override `supported-systems.nix` original content to be able to use `nix flake show|check` on dev machines (workaround for https://github.com/NixOS/nix/issues/4265)'
      nixdir=$(find_up "supported-systems.nix")
      SYSTEMS="$nixdir/supported-systems.nix"
      BACKUP="$(mktemp)"
      mv "$SYSTEMS" "$BACKUP"
      echo '[ "${system}" ]' > "$SYSTEMS"
      function atexit() {
          mv "$BACKUP" "$SYSTEMS"
      }
      trap atexit EXIT
    fi
    GC_DONT_GC=1 ${nixFlakes}/bin/nix --experimental-features "nix-command flakes" "$@"
  '';
  cabalWrapped = writeShellScriptBin "cabal" ''
    set -euo pipefail

    find_up() {
      while [[ $PWD != / ]] ; do
        if [[ -e "$1" ]]; then
          echo "$PWD"
          return
        fi
        cd ..
      done
    }

    toplevel=$(find_up "cabal.project")

    if [[ -n "$toplevel" ]]; then
      cabal_project="$toplevel/cabal.project"
      nix_shell_cabal_project=$toplevel/.nix-shell-cabal.project
      extra_cabal_opts=("--project-file=$nix_shell_cabal_project")
      awk '
        # Add comment with explanation of file
        BEGIN {
          print "-- Generated from '"$cabal_project"' by the wrapper script"
          print "-- ${placeholder "out"}"
          print "-- Add this file to your .gitignore\n"
        }

        # Matches all section starts
        /^[^ ]/ {
          # Remember the section name (they can span multiple lines)
          section = $0
        }
        # Matches every line
        // {
          # Only print the line if it is not in the section we want to omit
          if (section != "source-repository-package")
            print $0
        }
      ' "$cabal_project" > "$nix_shell_cabal_project"
    else
      extra_cabal_opts=()
    fi

    cabal=${final.cabal or final.cabal-install}/bin/cabal
    >&2 echo "$cabal ''${extra_cabal_opts[@]} $@"
    exec "$cabal" "''${extra_cabal_opts[@]}" "$@"
  '';

  hlintCheck = ../../tests/hlint.nix;
  shellCheck = ../../tests/shellcheck.nix;
  stylishHaskellCheck = ../../tests/stylish-haskell.nix;

  checkStackProject = final.callPackage ../../ci/check-stack-project.nix { };
  sha256mapRegenerate = final.callPackage ../../ci/sha256map-regenerate { };

}
