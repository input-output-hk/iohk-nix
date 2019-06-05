{ substituteAll
, jq
, hydra
, lib
, coreutils
, time
, gnutar
, gzip
}:
let
  check-hydra = substituteAll {
    src = ./check-hydra.sh;
    tools = lib.makeBinPath [ jq hydra coreutils time gnutar gzip ];
    isExecutable = true;
    postInstall = ''
      patchShebangs $out
    '';
  };
in check-hydra
