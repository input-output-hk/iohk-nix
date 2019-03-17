# This tool is useful for Stack builds on Buildkite.
{ stdenv, fetchurl, zlib, gmp }:

stdenv.mkDerivation rec {
  name = "cache-s3-${version}";
  version = "v0.1.7";
  src = fetchurl {
    url = "https://github.com/fpco/cache-s3/releases/download/${version}/cache-s3-${version}-linux-x86_64.tar.gz";
    sha256 = "1fg2j5px88kmjv1knlvjlkfp2bv379vw47qf1262i3aamysz5kqp";
  };
  libPath = stdenv.lib.makeLibraryPath [
    stdenv.cc.cc.lib
    zlib
    gmp
  ];
  sourceRoot = ".";
  buildPhase = "true";
  installPhase = ''
    mkdir -p $out/bin
    install -m 0755 cache-s3 $out/bin
  '';
  postFixup = ''
    patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" $out/bin/cache-s3
    patchelf --set-rpath ${libPath} $out/bin/cache-s3
  '';
}
