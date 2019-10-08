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
    rev = "0459acacce4bf5994d1eb0b4234318c691411f84";
    sha256 = "1g76q98wrgaz3s97ki48wy7v0a357griwwh8hhrr6qm2fb4k6afm";
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
