{ stdenv, lib, autoreconfHook, src }:

stdenv.mkDerivation rec {
  pname = "libsodium-vrf";
  version = "1.0.18";

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

  # Verify .pc version matches derivation version
  preCheck = ''
    pc_file="libsodium.pc"
    if [ ! -f "$pc_file" ]; then
      echo "ERROR: pkg-config file not found: $pc_file"
      exit 1
    fi
    pc_version=$(sed -n 's/^Version: *//p' "$pc_file")
    if [ "$pc_version" != "${version}" ]; then
      echo "ERROR: Version mismatch: derivation has ${version}, but .pc file has $pc_version"
      exit 1
    fi
  '';

  meta = with lib; {
    description = "A modern and easy-to-use crypto library - VRF fork";
    homepage = "http://doc.libsodium.org/";
    license = licenses.isc;
    maintainers = [ "tdammers" "nclarke" ];
    platforms = platforms.all;
  };
}
