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
    version = "0.7.0-rc4";
    sha256 = "1cjdapy0r2bikqck64cl09vzs307wcfi628hfmpczrg33i81pr3g";
    cargoSha256 = "0546ahgwcczaxda1hc1r20skzi93s40isq2ys40y9165sgdydn4i";
  };

  master = {
    name = "jormungandr-master";
    version = "master";
    rev = "a4692b762f4b8d92a335f70f5acdfc7afcad3fe8";
    sha256 = "03ijiqhsv17px2lnpzq3bvpq3dy8pmlqdrda100l3mkk1qb7ayjc";
    cargoSha256 = "1pp829azj6aw68ba637rm852sj61nxznxfzrlqs9ds6adk9h7abs";
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
