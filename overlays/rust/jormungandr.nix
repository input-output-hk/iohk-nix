{ rustPlatform
, fetchFromGitHub
, sqlite
, protobuf
, pkgconfig
, openssl
,  ... }:

rustPlatform.buildRustPackage rec {
  version = "0.2.1";
  name = "jormungandr-${version}";
  src = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "jormungandr";
    rev = "v${version}";
    sha256 = "1rq40968vznix2lmkvn9c3ap65ncjnqv5wxmhrr0vj6sa2m24ikd";
    fetchSubmodules = true;
  };

  cargoSha256 = "0kpfq3j8wgsw685p94zk57h26zivnvrshvrfx35blb6aabr6kmx8";
  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ sqlite protobuf openssl ];
  PROTOC = "${protobuf}/bin/protoc";
  JOR_CLI_NAME = "../release/jcli";
  JORMUNGANDR_NAME = "../release/jormungandr";
}
