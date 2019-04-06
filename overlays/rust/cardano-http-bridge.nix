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
    owner = "input-output-hk";
    repo = "cardano-http-bridge";
    rev = "ea4eebe63d44631f46818aa5f0531448f29872fe";
    sha256 = "1gjf6rgf2rkz4aabg279nkpka6kb0ppb1m7gq5a3rlrx5ss9agd3";
    fetchSubmodules = true;
  };

  cargoSha256 = "19g5fy8af65vd9rl66058c67nlrz7r6mjd0cy83865d7q81hdl8r";
  buildInputs = [ sqlite protobuf pkgconfig openssl ];
  PROTOC = "${protobuf}/bin/protoc";
}
