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
  version = "0.3.1";
  sha256 = "0ys8sw73c7binxnl79dqi7sxva62bgifbhgyzvvjvmjjdxgq4kfp";
  cargoSha256 = "0fphjzz78ym15qbka01idnq6vkyf4asrnhrhvxngwc3bifmnj937";
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
}
