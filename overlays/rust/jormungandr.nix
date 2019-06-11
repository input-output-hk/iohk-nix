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
    rev = "88f3421b0665f36782a70485fce4922935e785dc";
    sha256 = "0xcg2vzl5zc0hzq339r4sxc2nzj9vc28xhplcfxf1k1x0xis79yq";
    fetchSubmodules = true;
  };

  cargoSha256 = "1na2fqf90x34c2qix2i4yjdimjb3k6khfk7m1ig5vqc5nv6y7ahy";
  nativeBuildInputs = [ pkgconfig ];
  buildInputs = [ sqlite protobuf openssl ];
  PROTOC = "${protobuf}/bin/protoc";
}
