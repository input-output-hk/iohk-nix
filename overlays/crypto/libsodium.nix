{ stdenv, lib, fetchFromGitHub, autoreconfHook }:

stdenv.mkDerivation rec {
  name = "libsodium-1.0.18";

  src = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "libsodium";
    rev = "66f017f16633f2060db25e17c170c2afa0f2a8a1";
    sha256 = "12g2wz3gyi69d87nipzqnq4xc6nky3xbmi2i2pb2hflddq8ck72f";
  };

  nativeBuildInputs = [ autoreconfHook ];

  configureFlags = "--enable-static";

  outputs = [ "out" "dev" ];
  separateDebugInfo = stdenv.isLinux && stdenv.hostPlatform.libc != "musl";

  enableParallelBuilding = true;

  doCheck = true;

  meta = with lib; {
    description = "A modern and easy-to-use crypto library";
    homepage = "http://doc.libsodium.org/";
    license = licenses.isc;
    maintainers = [ "tdammers" "nclarke" ];
    platforms = platforms.all;
  };
}
