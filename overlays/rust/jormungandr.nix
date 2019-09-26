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
  release = {
    version = "0.5.1";
    sha256 = "1zdy0yymsipk0a5kcjnkpnzllnx27xvd6331a4w231azj5x1gif2";
    cargoSha256 = "1grrxmczdmkf2sd0f0b2iblzzcp8qlrsad5dkm0r5vxch22rcx7d";
  };

  master = {
    name = "jormungandr-master";
    version = "master";
    rev = "1de8ae2cd88b58a0268f8bc6ff8cdbef9c61365b";
    sha256 = "1d673l94npa7i8zkw1fwjv5nb6y41f0kbdaw7nq7x55vsnxjg36m";
    cargoSha256 = "1grrxmczdmkf2sd0f0b2iblzzcp8qlrsad5dkm0r5vxch22rcx7d";
  };

  fetchSrc = { name, rev, sha256 }:
    fetchFromGitHub {
      owner = "input-output-hk";
      repo = "jormungandr";
      inherit name rev sha256;
      fetchSubmodules = true;
    };

  Security = darwin.apple_sdk.frameworks.Security;

  makeJormungandr = { version, rev ? "v${version}", sha256, cargoSha256, name ? "jormungandr-${version}" }:
    rustPlatform.buildRustPackage rec {
      inherit name version;
      src = fetchSrc {
        name = "${name}-source";
        inherit rev sha256;
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
      passthru = { inherit src; };
    };

  makeJcli = { version, rev ? "v${version}", sha256, cargoSha256, name ? "jormungandr-cli-${version}" }:
    rustPlatform.buildRustPackage rec {
      inherit name version;
      src = fetchSrc {
        name = "${name}-source";
        inherit rev sha256;
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
      passthru = { inherit src; };
    };

in {
  inherit makeJormungandr makeJcli;

  jormungandr = makeJormungandr release;
  jcli = makeJcli release;
  jormungandr-master = makeJormungandr master;
  jcli-master = makeJcli master;
}
