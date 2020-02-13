{ stdenv, cardano-repo-tool }:

stdenv.mkDerivation {
  name = "stack-cabal-sync-shell";
  buildInputs = [ cardano-repo-tool ];
  shellHook = ''
    for EXE in cardano-repo-tool; do
      source <($EXE --bash-completion-script `type -p $EXE`)
    done
  '';
  phases = ["nobuildPhase"];
  nobuildPhase = "mkdir -p $out";
}
