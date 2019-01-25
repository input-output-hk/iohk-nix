# This file contains patches across haskell packages that
# amend the packages to be cross-compilable to windows,
# and potentially later other targets.
#
# The module is supposed to be used as part of the pkgSet's
# `modules` setion:
# Example:
#    modules = [
#      haskell.ghcHackagePatches.${(stack-pkgs.overlay hackage).compiler.nix-name}
#      iohk-module
#    ];
#
commonLib:
{ pkgs, buildModules, config, lib, ... }:
let
        withTH = import ./mingw_w64.nix {
          inherit (commonLib.pkgs) stdenv lib writeScriptBin;
          wine = pkgs.buildPackages.winePackages.minimal;
          inherit (pkgs.windows) mingw_w64_pthreads;
          inherit (pkgs) gmp;
          # iserv-proxy needs to come from the buildPackages, as it needs to run on the
          # build host.
          inherit (config.hsPkgs.buildPackages.iserv-proxy.components.exes) iserv-proxy;
          # remote-iserv however needs to come from the regular packages as it has to
          # run on the target host.
          inherit (config.hsPkgs.remote-iserv.components.exes) remote-iserv;
          # we need to use openssl.bin here, because the .dll's are in the .bin expression.
          extra-test-libs = [ pkgs.rocksdb pkgs.openssl.bin ];

        } // { doCrossCheck = true; };
in {
  # clock hasn't had a release since 2016(!) that is for three(3) years
  # now.
  packages.clock.patches = [ ({ version, revision }: if version == "0.7.2" then ./patches/clock-0.7.2.patch else null) ];
  # nix calles this package crypto
  packages.cryptonite-openssl.patches = [ ({ version, revision }: if version == "0.7" then ./patches/cryptonite-openssl-0.7.patch else null) ];

  packages.conduit.patches            = [ ./patches/conduit-1.3.0.2.patch ];
  packages.streaming-commons.patches  = [ ./patches/streaming-commons-0.2.0.0.patch ];
  packages.x509-system.patches        = [ ./patches/x509-system-1.6.6.patch ];
  packages.file-embed-lzma.patches    = [ ./patches/file-embed-lzma-0.patch ];

} // lib.optionalAttrs commonLib.pkgs.stdenv.hostPlatform.isWindows  {
         packages.hedgehog               = withTH;
         packages.cardano-crypto-wrapper = withTH;
}
