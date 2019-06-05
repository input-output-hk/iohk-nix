{ rustPlatform
, fetchFromGitHub
, sqlite
, protobuf
, pkgconfig
, openssl
,  ... }:

rustPlatform.buildRustPackage rec {
  version = "0.1.0";
  name = "jormungandr-${version}";
  src = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "jormungandr";
    rev = "c53a896774d30b357a6dbbabe909a14509fa078a";
    sha256 = "1pwkb8xrmzpwnr5fw1yns7m3qybpc67na3kzwsvqcb4xbnb57yyy";
    fetchSubmodules = true;
  };

  cargoSha256 = "1n0qg0nqhs653xc0d2iy4dd0shzbdxklxydxh674dkssrb28bmab";
  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ sqlite protobuf openssl ];
  PROTOC = "${protobuf}/bin/protoc";
}
