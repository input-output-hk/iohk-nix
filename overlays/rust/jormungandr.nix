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
    version = "0.2.1";
    name = "jormungandr-${version}";
    src = fetchFromGitHub {
      owner = "input-output-hk";
      repo = "jormungandr";
      rev = "v${version}";
      sha256 = "1rq40968vznix2lmkvn9c3ap65ncjnqv5wxmhrr0vj6sa2m24ikd";
      fetchSubmodules = true;
    };

    cargoSha256 = "0kpfq3j8wgsw685p94zk57h26zivnvrshvrfx35blb6aabr6kmx8";
    nativeBuildInputs = [ pkgconfig ];
    buildInputs = [ sqlite protobuf openssl ] ++ stdenv.lib.optional stdenv.isDarwin Security;
    PROTOC = "${protobuf}/bin/protoc";
    JOR_CLI_NAME = "../release/jcli";
    JORMUNGANDR_NAME = "../release/jormungandr";
  };

  jormungandrMaster = rustPlatform.buildRustPackage rec {
    version = "0.2.1";
    name = "jormungandr-master-${version}";
    src = fetchFromGitHub {
      owner = "input-output-hk";
      repo = "jormungandr";
      rev = "bebd3a3ad8b4065e2b2744586c89d69cc20c2adb";
      sha256 = "0dznwql27kd9ygvhy5vc0wbqkhy9jj5nbpa4c9zwln48r6xvznyg";
      fetchSubmodules = true;
    };
    doCheck = false;
    cargoSha256 = "15xdhvxish67rp48idsi6qc669skyl0xrn9a6b6ql563j934a70s";
    nativeBuildInputs = [ pkgconfig ];
    buildInputs = [ sqlite protobuf openssl ] ++ stdenv.lib.optional stdenv.isDarwin Security;
    PROTOC = "${protobuf}/bin/protoc";
    JOR_CLI_NAME = "../release/jcli";
    JORMUNGANDR_NAME = "../release/jormungandr";
  };
}