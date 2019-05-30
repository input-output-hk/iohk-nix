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
    rev = "6c68850c1d057b3414f00cba32f48410b97f516a";
    sha256 = "0la0c4pz3wis6y36j3bb84rsvchmbwsf00mhp6v3n28aqw8zmvnp";
    fetchSubmodules = true;
  };

  cargoSha256 = "1bddbx8mgbgbq944xm4gjvbxm5kynj28r0lz89ayd85nds5v59dc";
  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ sqlite protobuf openssl ];
  PROTOC = "${protobuf}/bin/protoc";
}
