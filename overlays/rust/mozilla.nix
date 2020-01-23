{ fetchFromGitHub }:
let
  rustOverlay = fetchFromGitHub {
    owner = "mozilla";
    repo = "nixpkgs-mozilla";
    rev = "5300241b41243cb8962fad284f0004afad187dad";
    sha256 = "1h3g3817anicwa9705npssvkwhi876zijyyvv4c86qiklrkn5j9w";
  };
in
  import (rustOverlay + "/rust-overlay.nix")
