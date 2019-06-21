{ rustPlatform
, stdenv
, fetchFromGitHub
, sqlite
, protobuf
, pkgconfig
, openssl
, darwin
, rustup
, rustc
, cargo
, rustfmt
,  ... }:
let
  Security = darwin.apple_sdk.frameworks.Security;
in {

  jormungandr = rustPlatform.buildRustPackage rec {
    version = "0.2.2";
    name = "jormungandr-${version}";
    src = fetchFromGitHub {
      owner = "input-output-hk";
      repo = "jormungandr";
      rev = "v${version}";
      sha256 = "0fy6sq2j9lxkn7md094ysildwcsnjjq142js4l8x0f1l8yfpqh3i";
      fetchSubmodules = true;
    };

    cargoSha256 = "0kpffffffffw685p94zk57h26zivnvrshvrfff5blb6aabr6kmx8";
    nativeBuildInputs = [ pkgconfig ];
    buildInputs = [ sqlite protobuf openssl ] ++ stdenv.lib.optional stdenv.isDarwin Security;
    PROTOC = "${protobuf}/bin/protoc";
    JOR_CLI_NAME = "../release/jcli";
    JORMUNGANDR_NAME = "../release/jormungandr";
  };

  jormungandrMaster = rustPlatform.buildRustPackage rec {
    version = "unstable";
    name = "jormungandr-master-${version}";
    src = fetchFromGitHub {
      owner = "input-output-hk";
      repo = "jormungandr";
      rev = "d471df6b609484c65538f97dd36f925ab8c6812a";
      sha256 = "0fy6sq2j9lxkn7md094ysildwcsnjjq142js4l8x0f1l8yfpqh3i";
      fetchSubmodules = true;
    };
    doCheck = false;
    cargoSha256 = "0sc86cmzii3dm3665sl68d9a1mzb7ffff0f1b9d60jcb60hbbg7m";
    nativeBuildInputs = [ pkgconfig ];
    buildInputs = [ sqlite protobuf openssl ] ++ stdenv.lib.optional stdenv.isDarwin Security;
    PROTOC = "${protobuf}/bin/protoc";
    JOR_CLI_NAME = "../release/jcli";
    JORMUNGANDR_NAME = "../release/jormungandr";
  };
}
