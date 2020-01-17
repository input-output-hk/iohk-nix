{ substituteAll
, jq
, lib
, coreutils
, time
, gnutar
, gzip
, fetchFromGitHub
}:
let
  rev = "0768891e3cd3ef067d28742098f1dea8462fca75";
  hydra-src = fetchFromGitHub {
    inherit rev;
    owner = "input-output-hk";
    repo = "hydra";
    sha256 = "1aw3p7jm2gsakdqqx4pzhkfx12hh1nxk3wkabcvml5ci814f6jic";
  };
  hydra = (import "${hydra-src}/release.nix" {
    hydraSrc = {
      outPath = hydra-src;
      rev = builtins.substring 0 6 rev;
      revCount = 1234;
    };
  }).build.x86_64-linux;
  check-hydra = substituteAll {
    src = ./check-hydra.sh;
    tools = lib.makeBinPath [ jq hydra coreutils time gnutar gzip ];
    isExecutable = true;
    postInstall = ''
      patchShebangs $out
    '';
  };
in check-hydra
