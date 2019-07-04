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
    version = "0.2.3";
    name = "jormungandr-${version}";
    src = fetchFromGitHub {
      owner = "input-output-hk";
      repo = "jormungandr";
      rev = "v${version}";
      sha256 = "1z87vskrck2xk8jvpr7550bxmm8dbidvlcwa4q2wfp4nhacwhfkk";
      fetchSubmodules = true;
    };

    cargoSha256 = "1svjz14hwg6b2xd7rhn1n43d0mgdp1vb0x3bjj60yqvxz12mwl1s";
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
    version = "0.2.3";
    name = "jormungandr-cli-${version}";
    src = fetchFromGitHub {
      owner = "input-output-hk";
      repo = "jormungandr";
      rev = "v${version}";
      sha256 = "1z87vskrck2xk8jvpr7550bxmm8dbidvlcwa4q2wfp4nhacwhfkk";
      fetchSubmodules = true;
    };

    cargoSha256 = "1svjz14hwg6b2xd7rhn1n43d0mgdp1vb0x3bjj60yqvxz12mwl1s";
    nativeBuildInputs = [ pkgconfig ];
    buildInputs = [ sqlite protobuf openssl ]
      ++ lib.optional stdenv.isDarwin Security
      ++ lib.optional stdenv.isLinux systemd;
    preBuild = "cd jcli";
    preInstall = "cd ..";
    postInstall = ''
      mkdir $out/scripts
      cp scripts/* $out/scripts/
    '';
    PROTOC = "${protobuf}/bin/protoc";
  };
}
