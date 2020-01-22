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
    rev = "31cd4d48b3b65ef8d2d10c493a1191535b785923";
    sha256 = "0xfbqaj9ksfx704pb65pccalhfx2p2z2lg4rsg12n58g3ih449k3";
    fetchSubmodules = true;
  };

  cargoSha256 = "07l3gbqzgv29zxpv7rmq1wkkyi3yw3w7j10bf0ws8fv92k9j922x";
  buildInputs = [ protobuf ] ++ stdenv.lib.optional stdenv.isDarwin Security;
  PROTOC = "${protobuf}/bin/protoc";
  # workaround https://github.com/NixOS/nixpkgs/issues/61618
  preConfigure = ''
    export HOME=`mktemp -d`
  '';
}
