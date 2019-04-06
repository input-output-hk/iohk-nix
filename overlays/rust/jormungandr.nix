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
    rev = "412902c214cf86959a9db2b1bf3ca6019f05cd58";
    sha256 = "0r7jv4zlwq0nqb0i70dknqbm8s4q01ka3rskimmzqgivbhggvjxz";
    fetchSubmodules = true;
  };

  cargoSha256 = "1pa85b20k83ann7kplx5gr3r5g0793wd7m6xpl2hb0hi2v6a2b7k";
  buildInputs = [ sqlite protobuf pkgconfig openssl ];
  PROTOC = "${protobuf}/bin/protoc";
}
