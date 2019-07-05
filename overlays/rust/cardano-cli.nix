{ rustPlatform
, fetchFromGitHub
, sqlite
, protobuf
, pkgconfig
, openssl
,  ... }:

rustPlatform.buildRustPackage rec {
  version = "0.0";
  name = "cardano-cli-${version}";
  src = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-cli";
    rev = "ed064d5a3b96c23b52bb20ca49da9cb8764a2e0f";
    sha256 = "07y5ssar6aq93snrvmapk05zmym4w23ydvjn2njp8saxk23ivqsg";
    fetchSubmodules = true;
  };

  cargoSha256 = "1jra0635inm95xp7vg8l7s7ybijcy28956cd6jl8qw2p8bw0z1p3";
  buildInputs = [ protobuf ];
  PROTOC = "${protobuf}/bin/protoc";
  # workaround https://github.com/NixOS/nixpkgs/issues/61618
  preConfigure = ''
    export HOME=`mktemp -d`
  '';
}
