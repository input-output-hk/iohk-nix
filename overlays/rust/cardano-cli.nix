{ rustPlatform
, fetchFromGitHub
, sqlite
, protobuf
, pkgconfig
, openssl
,  ... }:

rustPlatform.buildRustPackage rec {
  version = "0.0";
  name = "cardano-cli-${version}";
  src = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-cli";
    rev = "268f77ad2189122ae9fe0f296d2501b03fedabf3";
    sha256 = "11ybkf03zdj0iyc3aq6iakab31vz6r6rdy619p3mkmm9h7wrx0j3";
    fetchSubmodules = true;
  };

  cargoSha256 = "1kspfs89021skayh53fldcj24mcc6z9wx9crhqbrjlwnr4yx4ry9";
  buildInputs = [ sqlite protobuf pkgconfig openssl ];
  PROTOC = "${protobuf}/bin/protoc";
}
