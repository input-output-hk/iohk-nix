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
    version = "0.7.0-rc6";
    sha256 = "0arszp33pahhg11k19gn9wajyqhlwnkfpmgydjzdjycfiva218fx";
    cargoSha256 = "03zgj0vnw4j63bbim4v0ahxsd91smpsg6swv313m117gdmmy8zqm";
  };

  master = {
    name = "jormungandr-master";
    version = "master";
    rev = "83fe55ce84e2f4499419930c868c2d4a92019fcb";
    sha256 = "06awmnfya1qzlx6gywq8hldn1hgp2j5713qkmlxs59nwwnskmd72";
    cargoSha256 = "03zgj0vnw4j63bbim4v0ahxsd91smpsg6swv313m117gdmmy8zqm";
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
