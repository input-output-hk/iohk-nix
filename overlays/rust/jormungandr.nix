{ lib
, stdenv
, rustPlatform
, fetchFromGitHub
, sqlite
, protobuf
, pkgconfig
, openssl
,  ... }:

rustPlatform.buildRustPackage rec {
  version = "0.1.0";
  name = "jormungandr-${version}";
  src = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "jormungandr";
    rev = "3e5e425d232ec342ee45d86bd133fec149263ad2";
    sha256 = "1iyamy29dniav4qssc45gp15nv28baa3f0vr41dbgxxnpyfxr2f6";
    fetchSubmodules = true;
  };

  cargoBuildFlags = lib.optionals (stdenv.isLinux) ["--features jormungandr/systemd"];
  cargoSha256 = "1na2fqf90x34c2qix2i4yjdimjb3k6khfk7m1ig5vqc5nv6y7ahy";
  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ sqlite protobuf openssl ];
  PROTOC = "${protobuf}/bin/protoc";
  JOR_CLI_NAME = "../release/jcli";
  JORMUNGANDR_NAME = "../release/jormungandr";
}
