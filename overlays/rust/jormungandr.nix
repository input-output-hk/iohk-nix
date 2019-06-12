{ rustPlatform
, fetchFromGitHub
, sqlite
, protobuf
, pkgconfig
, openssl
,  ... }:

rustPlatform.buildRustPackage rec {
  version = "0.1.0";
  name = "jormungandr-${version}";
  src = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "jormungandr";
    rev = "6522b8da77d38db1844186dea74bc3bc0c2f516c";
    sha256 = "1sl2bw5x1flxzyrk9sxqhwnlbh4bqjcgnas915dfqwsali8nijsi";
    fetchSubmodules = true;
  };

  cargoSha256 = "1na2fqf90x34c2qix2i4yjdimjb3k6khfk7m1ig5vqc5nv6y7ahy";
  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ sqlite protobuf openssl ];
  PROTOC = "${protobuf}/bin/protoc";
  JOR_CLI_NAME = "../release/jcli";
  JORMUNGANDR_NAME = "../release/jormungandr";
}
