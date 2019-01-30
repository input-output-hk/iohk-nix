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
# we need nixpkgs as this will be the properly configured one
# the one that gives us the right host and target platforms.
nixpkgs:
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
  packages = {
    # This needs true, otherwise we miss most of the interesting
    # modules.
    ghci.flags.ghci = true;

    # this needs to be true to expose module
    #  Message.Remote
    # as needed by libiserv.
    libiserv.flags.network = true;

    # libiserv has a bit too restrictive boundaries.
    # as such it won't build with newer network libraries.
    # to avoid that we use doExactConfig, which forces cabal
    # to forgoe its solver and just take the libraries it's
    # provided with.
    ghci.components.library.doExactConfig = true;
    libiserv.components.library.doExactConfig = true;
    # same for iserv-proxy
    iserv-proxy.components.exes.iserv-proxy.doExactConfig = true;
    remote-iserv.components.exes.remote-iserv.doExactConfig = true;

    #ghci.components.library.doExactConfig = true;

    # clock hasn't had a release since 2016(!) that is for three(3) years
    # now.
    clock.patches              = [ ({ version, revision }: (if version == "0.7.2" then ./patches/clock-0.7.2.patch else null)) ];
    # nix calles this package crypto
    cryptonite-openssl.patches = [ ({ version, revision }: if version == "0.7" then ./patches/cryptonite-openssl-0.7.patch else null) ];

    conduit.patches            = [ ./patches/conduit-1.3.0.2.patch ];
    streaming-commons.patches  = [ ./patches/streaming-commons-0.2.0.0.patch ];
    x509-system.patches        = [ ./patches/x509-system-1.6.6.patch ];
    file-embed-lzma.patches    = [ ./patches/file-embed-lzma-0.patch ];
  } // lib.optionalAttrs nixpkgs.stdenv.hostPlatform.isWindows {
    hedgehog               = withTH;
    cardano-crypto-wrapper = withTH;
    cardano-chain          = withTH;
  };
}
