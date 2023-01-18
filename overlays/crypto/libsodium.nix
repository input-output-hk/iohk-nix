{ stdenv, lib, fetchFromGitHub, autoreconfHook }:

stdenv.mkDerivation rec {
  name = "libsodium-1.0.18";

  src = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "libsodium";
    rev = "a5baeccc5be38dda54dd85eba58fdd929b2b8387";
    sha256 = "0cgml4b7365q87385xxi0q0882b82d3x1whp6i7qa7x4w1gvgqk0";
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
