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

    cargoSha256 = "1svjz14hwg6b2xd7rhn1n43d0mgdp1vb0x3bjj60yqvxz12mwl1s";
    cargoPatches = [ ./patches/jormungandr-0.2.2-generate-lockfile.patch ];
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
    cargoSha256 = "1svjz14hwg6b2xd7rhn1n43d0mgdp1vb0x3bjj60yqvxz12mwl1s";
    cargoPatches = [ ./patches/jormungandr-0.2.2-generate-lockfile.patch ];
    nativeBuildInputs = [ pkgconfig ];
    buildInputs = [ sqlite protobuf openssl ] ++ stdenv.lib.optional stdenv.isDarwin Security;
    PROTOC = "${protobuf}/bin/protoc";
    JOR_CLI_NAME = "../release/jcli";
    JORMUNGANDR_NAME = "../release/jormungandr";
  };
}
