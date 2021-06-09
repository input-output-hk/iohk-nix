final: prev: let
  inherit (final) system git nixFlakes cabal writeShellScriptBin;
in {
  nixWrapped = writeShellScriptBin "nix" ''
    if [[ "$@" == "flake show"* ]] || [[ "$@" == "flake check"* ]]; then
      >&2 echo 'Temporary override `supported-systems.nix` original content to be able to use `nix flake show|check` on dev machines (workaround for https://github.com/NixOS/nix/issues/4265)'
      SYSTEMS="$(${git}/bin/git rev-parse --show-toplevel)/supported-systems.nix"
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
    >&2 echo 'Temporary modify `cabal.project` to use nix builds of `source-repository-package`s (haskell/cabal#5444 and haskell/cabal#6249).'
    PROJECT="$(${git}/bin/git rev-parse --show-toplevel)/cabal.project"
    BACKUP="$(mktemp)"
    cp -a "$PROJECT" "$BACKUP"
    sed -ni '1,/--- 8< ---/ p' $PROJECT
    function atexit() {
        mv "$BACKUP" "$PROJECT"
    }
    trap atexit EXIT
    ${final.cabal or final.cabal-install}/bin/cabal "$@"
  '';

  hlintCheck = ../../tests/hlint.nix;
  shellCheck = ../../tests/shellcheck.nix;
  stylishHaskellCheck = ../../tests/stylish-haskell.nix;

}
