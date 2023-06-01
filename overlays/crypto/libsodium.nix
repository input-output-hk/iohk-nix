{ stdenv, lib, autoreconfHook, src }:

stdenv.mkDerivation rec {
  pname = "libsodium-vrf";
  version = src.shortRev;

  inherit src;

  nativeBuildInputs = [ autoreconfHook ];

  configureFlags = [ "--enable-static" ]
    # Fixes a compilation failure: "undefined reference to `__memcpy_chk'". Note
    # that the more natural approach of adding "stackprotector" to
    # `hardeningDisable` does not resolve the issue.
    ++ lib.optional stdenv.hostPlatform.isMinGW "CFLAGS=-fno-stack-protector";

  outputs = [ "out" "dev" ];
  separateDebugInfo = stdenv.isLinux && stdenv.hostPlatform.libc != "musl";

  enableParallelBuilding = true;

  doCheck = true;

  meta = with lib; {
    description = "A modern and easy-to-use crypto library - VRF fork";
    homepage = "http://doc.libsodium.org/";
    license = licenses.isc;
    maintainers = [ "tdammers" "nclarke" ];
    platforms = platforms.all;
  };
}
