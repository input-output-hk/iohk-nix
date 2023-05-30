{ lib, stdenv, autoreconfHook, src,
  enableStatic ? stdenv.hostPlatform.isStatic }:

stdenv.mkDerivation rec {
  pname = "secp256k1";
  version = src.shortRev;

  inherit src;

  nativeBuildInputs = [ autoreconfHook ];

  configureFlags = [
    "--enable-benchmark=no"
    "--enable-module-recovery"
  ] ++ lib.optional enableStatic [
    "--enable-static"
  ];

  doCheck = true;

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
