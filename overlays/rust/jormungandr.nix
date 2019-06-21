{ rustPlatform
, lib
, stdenv
, fetchFromGitHub
, sqlite
, protobuf
, pkgconfig
, openssl
, systemd
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

    cargoSha256 = "15xdhvxish67rp48idsi6qc669skyl0xrn9a6b6ql563j934a70s";
    cargoPatches = [ ./patches/0001-update-Cargo.lock.patch ];
    nativeBuildInputs = [ pkgconfig ];
    buildInputs = [ sqlite protobuf openssl ]
      ++ lib.optional stdenv.isDarwin Security
      ++ lib.optional stdenv.isLinux systemd;
    preBuild = "cd jormungandr";
    preInstall = "cd ..";
    cargoBuildFlags = ["--features \"gelf"] ++ lib.optional stdenv.isLinux "systemd" ++ ["\""];
    PROTOC = "${protobuf}/bin/protoc";
  };

  jcli = rustPlatform.buildRustPackage rec {
    version = "0.2.2";
    name = "jormungandr-cli-${version}";
    src = fetchFromGitHub {
      owner = "input-output-hk";
      repo = "jormungandr";
      rev = "v${version}";
      sha256 = "0fy6sq2j9lxkn7md094ysildwcsnjjq142js4l8x0f1l8yfpqh3i";
      fetchSubmodules = true;
    };

    cargoSha256 = "15xdhvxish67rp48idsi6qc669skyl0xrn9a6b6ql563j934a70s";
    cargoPatches = [ ./patches/0001-update-Cargo.lock.patch ];
    nativeBuildInputs = [ pkgconfig ];
    buildInputs = [ sqlite protobuf openssl ]
      ++ lib.optional stdenv.isDarwin Security
      ++ lib.optional stdenv.isLinux systemd;
    preBuild = "cd jcli";
    preInstall = "cd ..";
    PROTOC = "${protobuf}/bin/protoc";
  };

  jormungandrMaster = rustPlatform.buildRustPackage rec {
    version = "unstable";
    name = "jormungandr-master-${version}";
    src = fetchFromGitHub {
      owner = "input-output-hk";
      repo = "jormungandr";
      rev = "cc02cc30c89f01fd807a4cb997a85744e228e829";
      sha256 = "0wfkgcvc3jvcr7xf18948925xhf46ijsh1m23zs5xz4r3j003llk";
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
