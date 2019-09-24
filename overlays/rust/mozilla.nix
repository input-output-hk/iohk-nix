{ fetchFromGitHub }:
let
  rustOverlay = fetchFromGitHub {
    owner = "mozilla";
    repo = "nixpkgs-mozilla";
    rev = "b52a8b7de89b1fac49302cbaffd4caed4551515f";
    sha256 = "1np4fmcrg6kwlmairyacvhprqixrk7x9h89k813safnlgbgqwrqb";
  };
in
  import (rustOverlay + "/rust-overlay.nix")
