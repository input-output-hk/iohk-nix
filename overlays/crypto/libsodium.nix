{ stdenv, lib, fetchFromGitHub, autoreconfHook }:

stdenv.mkDerivation rec {
  pname = "libsodium";
  version = "1.0.18";

  src = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "libsodium";
    rev = "dbb48cce5429cb6585c9034f002568964f1ce567";
    sha256 = "1rppbdq2x29mkias9wk225wadwqv59x65m9562xh6crgk0vmrr6j";
  };

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
