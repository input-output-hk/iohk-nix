{ lib, stdenv, autoreconfHook, src,
  enableStatic ? stdenv.hostPlatform.isStatic }:

stdenv.mkDerivation rec {
  pname = "secp256k1";
  version = "0.3.2";

  inherit src;

  nativeBuildInputs = [ autoreconfHook ];

  configureFlags = [
    "--enable-benchmark=no"
    "--enable-module-recovery"
  ] ++ lib.optional enableStatic [
    "--enable-static"
  ];

  doCheck = true;

  # Verify .pc version matches derivation version
  preCheck = ''
    pc_file="libsecp256k1.pc"
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
    description = "Optimized C library for EC operations on curve secp256k1";
    longDescription = ''
      Optimized C library for EC operations on curve secp256k1. Part of
      Bitcoin Core. This library is a work in progress and is being used
      to research best practices. Use at your own risk.
    '';
    homepage = "https://github.com/bitcoin-core/secp256k1";
    license = with licenses; [ mit ];
    maintainers = with maintainers; [ ];
    platforms = with platforms; all;
  };
}
