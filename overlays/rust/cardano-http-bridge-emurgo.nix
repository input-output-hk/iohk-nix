{ rustPlatform
, fetchFromGitHub
, sqlite
, protobuf
, pkgconfig
, openssl
,  ... }:

rustPlatform.buildRustPackage rec {
  version = "0.0";
  name = "cardano-http-bridge-${version}";
  src = fetchFromGitHub {
    owner = "Emurgo";
    repo = "cardano-http-bridge";
    rev = "33852c7d9eac91e3ececeed0886180fa68778909";
    sha256 = "0xyfciwry8xzjxxy4nvix7iyk7pdd5yydwvj1n9n58pajvz31d19";
    fetchSubmodules = true;
  };

  cargoSha256 = "0n5hlzaqvn5hd0dkg9kj82n4syb6f5k1zlqkbpbsfqrm2g7knp4c";
  buildInputs = [ protobuf ];
  PROTOC = "${protobuf}/bin/protoc";
}
