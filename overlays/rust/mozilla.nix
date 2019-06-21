{ fetchFromGitHub }:
let
  rustOverlay = fetchFromGitHub {
    owner = "mozilla";
    repo = "nixpkgs-mozilla";
    rev = "200cf0640fd8fdff0e1a342db98c9e31e6f13cd7";
    sha256 = "1am353ims43ylvay263alchzy3y87r1khnwr0x2fp35qr347bvxi";
  };
in
  import (rustOverlay + "/rust-overlay.nix")
