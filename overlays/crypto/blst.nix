{ stdenv, lib, fetchFromGitHub, autoreconfHook }:

stdenv.mkDerivation rec {
  name = "blst-0.3.10";

  src = fetchFromGitHub {
    owner = "supranational";
    repo = "blst";
    rev = "03b5124029979755c752eec45f3c29674b558446";
    sha256 = "01m28xxmm1x7riwyax7v9rycwl5isi06h2b2hl4gxnnylkayisn5";
  };

  buildPhase = "./build.sh";
  installPhase = ''
    mkdir -p $out/lib
    cp ./libblst.a $out/lib/
  '';

  enableParallelBuilding = true;

  doCheck = true;

  meta = with lib; {
    description = "Multilingual BLS12-381 signature library";
    homepage = "https://github.com/supranational/blst";
    license = licenses.isc;
    platforms = platforms.all;
  };
}
