{ rustPlatform
, fetchFromGitHub
, sqlite
, protobuf
, pkgconfig
, openssl
, stdenv
, darwin
,  ... }:

let
  Security = darwin.apple_sdk.frameworks.Security;
in rustPlatform.buildRustPackage rec {
  version = "0.0";
  name = "cardano-http-bridge-${version}";
  src = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-http-bridge";
    rev = "9215882375f7c1cfa1e4e0bc4bf74c4b8ff46a21";
    sha256 = "1z1bijkv70cyswbh0vjgfblq41jfddn82kj3fvd59x2y8mq3dv76";
    fetchSubmodules = true;
  };

  cargoSha256 = "19g5fy8af65vd9rl66058c67nlrz7r6mjd0cy83865d7q81hdl8r";
  buildInputs = [ protobuf ] ++ stdenv.lib.optional stdenv.isDarwin Security;
  PROTOC = "${protobuf}/bin/protoc";
}
