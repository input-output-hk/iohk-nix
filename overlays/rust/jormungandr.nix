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
    version = "0.2.4";
    name = "jormungandr-${version}";
    src = fetchFromGitHub {
      owner = "input-output-hk";
      repo = "jormungandr";
      rev = "b155772e6d8f72506b6b5f1dac79bdbca66fcddd";
      sha256 = "1rhlspsjy3b3isrfpvbqh17fh0szb4d1dd9s1y21nx22q9ykk8cf";
      fetchSubmodules = true;
    };

    cargoSha256 = "1y3h2rpidb95lxn2zfha8ihrjfw8gfqdnq0z5nkd3z16vlgi7k8g";
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
    version = "0.2.4";
    name = "jormungandr-cli-${version}";
    src = fetchFromGitHub {
      owner = "input-output-hk";
      repo = "jormungandr";
      rev = "b155772e6d8f72506b6b5f1dac79bdbca66fcddd";
      sha256 = "1rhlspsjy3b3isrfpvbqh17fh0szb4d1dd9s1y21nx22q9ykk8cf";
      fetchSubmodules = true;
    };

    cargoSha256 = "1y3h2rpidb95lxn2zfha8ihrjfw8gfqdnq0z5nkd3z16vlgi7k8g";
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
