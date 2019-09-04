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
  version = "0.3.1415";
  sha256 = "1kg20570ri3bj99gicpp2z272igj9s7m2qw0z1ndk60bxcyghi2x";
  cargoSha256 = "1rbissmf15c6vsip5rwfbzk75x19bcl532ynqvdj6arwj4662nd0";
  master = {
    rev = "25f780105656b5a6f81797635a817b6c93ec0bf3";
    sha256 = "0a5db051aaafx084nmwnc4j3cbnmfhb2rx5viiap6nrp7ca2yyfh";
    cargoSha256 = "1rbissmf15c6vsip5rwfbzk75x19bcl532ynqvdj6arwj4662nd0";
  };

in {

  jormungandr = rustPlatform.buildRustPackage rec {
    inherit version;
    name = "jormungandr-${version}";
    src = fetchFromGitHub {
      owner = "input-output-hk";
      repo = "jormungandr";
      rev = "v${version}";
      inherit sha256;
      fetchSubmodules = true;
    };

    inherit cargoSha256;
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

  jormungandr-master = rustPlatform.buildRustPackage rec {
    inherit version;
    name = "jormungandr-master";
    src = fetchFromGitHub {
      owner = "input-output-hk";
      repo = "jormungandr";
      inherit (master) rev sha256;
      fetchSubmodules = true;
    };

    inherit (master) cargoSha256;
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
    inherit version;
    name = "jormungandr-cli-${version}";
    src = fetchFromGitHub {
      owner = "input-output-hk";
      repo = "jormungandr";
      rev = "v${version}";
      inherit sha256;
      fetchSubmodules = true;
    };

    inherit cargoSha256;
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

  jcli-master = rustPlatform.buildRustPackage rec {
    inherit version;
    name = "jormungandr-cli-master";
    src = fetchFromGitHub {
      owner = "input-output-hk";
      repo = "jormungandr";
      inherit (master) rev sha256;
      fetchSubmodules = true;
    };

    inherit (master) cargoSha256;
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
