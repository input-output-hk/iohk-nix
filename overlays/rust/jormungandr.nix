{ rustPlatform
, fetchFromGitHub
, sqlite
, protobuf
, pkgconfig
, openssl
,  ... }:

rustPlatform.buildRustPackage rec {
  version = "0.0";
  name = "jormungandr-${version}";
  src = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "jormungandr";
    rev = "9cb52dfb9406b8579e6f98b44e30163799c5d6e9";
    sha256 = "0kblk3pm1i7l5cq43kammaljq5llnl2fvp5iqbblhjnzw3iq8fi7";
    fetchSubmodules = true;
  };

  cargoSha256 = "0ifbdd57m7xd8qdkq8cpfz1azq4j8rlg6j3ki9d25mb49l0563w9";
  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ sqlite protobuf openssl ];
  PROTOC = "${protobuf}/bin/protoc";
}
