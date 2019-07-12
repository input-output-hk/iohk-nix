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
    version = "0.3.0";
    name = "jormungandr-${version}";
    src = fetchFromGitHub {
      owner = "input-output-hk";
      repo = "jormungandr";
      rev = "2359f76111403bc721d300009f7a92b48d276c67";
      sha256 = "1p96aljnbqwhs11ycj9daa1a01nykf1bzvwkfln7lkls5x3xrin9";
      fetchSubmodules = true;
    };

    cargoSha256 = "0fphjzz78ym15qbka01idnq6vkyf4asrnhrhvxngwc3bifmnj937";
    nativeBuildInputs = [ pkgconfig ];
    buildInputs = [ sqlite protobuf openssl ]
      ++ lib.optional stdenv.isDarwin Security
      ++ lib.optional stdenv.isLinux systemd;
    preBuild = "cd jormungandr";
    preInstall = "cd ..";
    cargoBuildFlags = ["--features \"gelf"] ++ lib.optional stdenv.isLinux "systemd" ++ ["\""];
    PROTOC = "${protobuf}/bin/protoc";
    # workaround https://github.com/NixOS/nixpkgs/issues/61618
    preConfigure = ''
      export HOME=`mktemp -d`
    '';
  };

  jcli = rustPlatform.buildRustPackage rec {
    version = "0.3.0";
    name = "jormungandr-cli-${version}";
    src = fetchFromGitHub {
      owner = "input-output-hk";
      repo = "jormungandr";
      rev = "2359f76111403bc721d300009f7a92b48d276c67";
      sha256 = "1p96aljnbqwhs11ycj9daa1a01nykf1bzvwkfln7lkls5x3xrin9";
      fetchSubmodules = true;
    };

    cargoSha256 = "0fphjzz78ym15qbka01idnq6vkyf4asrnhrhvxngwc3bifmnj937";
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
    # workaround https://github.com/NixOS/nixpkgs/issues/61618
    preConfigure = ''
      export HOME=`mktemp -d`
    '';
  };
}
