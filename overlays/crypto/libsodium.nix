{ stdenv, lib, fetchFromGitHub, autoreconfHook }:

stdenv.mkDerivation rec {
  name = "libsodium-1.0.18";

  src = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "libsodium";
    rev = "cd731e523a3725283fcf55376cf83b27f6f0f335";
    sha256 = "1la3pkd07jkfqvbw7wrbi5mkjw58b9mcm82zqyv7qnq0vwa5piqq";
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
