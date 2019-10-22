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
    owner = "Emurgo";
    repo = "cardano-http-bridge";
    rev = "cd7fa514e8c17879663c41a046cb6bd51e6b18d0";
    sha256 = "1s7p1jy63myahkm90y2kgxik8kv311f6prwdsl8p3arjh5vg85x3";
    fetchSubmodules = true;
  };

  cargoSha256 = "0sr9mzw9d5j86f7j7y2hdaxdrl3jm4dyzmbhpxjb8x6nf1bi10an";
  buildInputs = [ protobuf ] ++ stdenv.lib.optional stdenv.isDarwin Security;
  PROTOC = "${protobuf}/bin/protoc";
  # workaround https://github.com/NixOS/nixpkgs/issues/61618
  preConfigure = ''
    export HOME=`mktemp -d`
  '';
}
