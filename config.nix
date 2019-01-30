{
  # allow building for windows
  allowUnsupportedSystem = true;
  # we want the 64bit wine version
  wine.build = "wine64";


  # sadly we need to patch GHC a bit.
  packageOverrides = ps: with ps; let

    ghcPkgOverrides = {
        ghcFlavour = if ps.stdenv.targetPlatform == ps.stdenv.hostPlatform
                     then "perf"
                     else if ps.stdenv.targetPlatform.isWindows
                          then "perf-cross-ncg"
                          else "perf-cross";
        enableShared = ps.stdenv.targetPlatform == ps.stdenv.hostPlatform;
        enableIntegerSimple = false;
      };
    ghcDrvOverrides = drv: {
        dontStrip = true;
        hardeningDisable = [ "stackprotector" "format" ];
        patches = (drv.patches or [])
         ++ lib.optional (builtins.compareVersions drv.version "8.6.3" == 0) ./patches/ghc/T16057--ghci-doa-on-windows.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.6" == -1) ./patches/ghc/move-iserv-8.4.2.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.6" == -1) ./patches/ghc/hsc2hs-8.4.2.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.6" == -1) ./patches/ghc/various-8.4.2.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.6" == -1) ./patches/ghc/lowercase-8.4.2.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.6" == -1) ./patches/ghc/cabal-exe-ext-8.4.2.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.6" == -1) ./patches/ghc/ghc-8.4.3-Cabal2201-SMP-test-fix.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.6" == -1) ./patches/ghc/outputtable-assert-8.4.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.5" ==  1) ./patches/ghc/outputtable-assert-8.6.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.5" ==  1) ./patches/ghc/mistuke-ghc-err_clean_up_error_handler-8ab1a89af89848f1713e6849f189de66c0ed7898.diff
         # this might be fixed in 8.6.4 (if a release is cut), or 8.8
         ++ lib.optional (builtins.compareVersions drv.version "8.5" ==  1
                       && builtins.compareVersions drv.version "8.8" == -1) ./patches/ghc/MR148--T16104-GhcPlugins.patch
         ++ [
          ./patches/ghc/iserv-proxy-cleanup.patch
          ./patches/ghc/lowercase-8.6.patch
          ./patches/ghc/dll-loader-8.4.2.patch
          ./patches/ghc/0001-Stop-the-linker-panic.patch
          ./patches/ghc/ghc-8.4.3-Cabal2201-no-hackage-tests.patch
          ./patches/ghc/ghc-8.4.3-Cabal2201-allow-test-wrapper.patch
          ./patches/ghc/ghc-8.4.3-Cabal2201-response-file-support.patch
          ./patches/ghc/ghc-8.6-Cabal-fix-datadir.patch
          ./patches/ghc/MR95--ghc-pkg-deadlock-fix.patch
          ./patches/ghc/MR196--ghc-pkg-shut-up.patch
         ];
        postPatch = (drv.postPath or "") + ''
        autoreconf
        '';
      };
  in rec {
   # use the pre-built rocksdb.
   # We seem to be unable to actually
   # build rocksdb with mingw64/gcc7
   # without producing a partial dud.
   #
   # Using the pre-built rocksdb, also
   # means we do not need the gcc7 hack
   # in our nixpkgs to allow mingw with
   # libwinpthreads.
   rocksdb = with ps.stdenv;
     if hostPlatform.isWindows
     then pkgs.callPackage ./pkgs/rocksdb-prebuilt.nix { inherit (buildPackages) fetchurl unzip; }
     else ps.rocksdb;

   # on windows we have this habit of putting libraries
   # into `bin`, wheras on unix it's usually `lib`. For
   # this confuses nix easily. So we'll just move the
   # .dll's from `bin` into `$out/lib`. Such that they
   # are trivially found.
   openssl = ps.openssl.overrideAttrs (drv: {
     postInstall = with ps.stdenv; drv.postInstall + lib.optionalString hostPlatform.isWindows ''
       cp $bin/bin/*.dll $out/lib/
     '';
   });
   mfpr = ps.mfpr.overrideAttrs (drv: {
     configureFlags = with ps.stdenv; (drv.configureFlags or []) ++ lib.optional hostPlatform.isWindows "--enable-static --disable-shared";
   });
   libmpc = ps.libmpc.overrideAttrs (drv: {
     configureFlags = with ps.stdenv; (drv.configureFlags or []) ++ lib.optional hostPlatform.isWindows "--enable-static --disable-shared";
   });

    haskell = lib.recursiveUpdate ps.haskell {
      compiler.ghc842 = (ps.haskell.compiler.ghc842.override ghcPkgOverrides).overrideAttrs ghcDrvOverrides;
      compiler.ghc843 = (ps.haskell.compiler.ghc843.override ghcPkgOverrides).overrideAttrs ghcDrvOverrides;
      compiler.ghc844 = (ps.haskell.compiler.ghc844.override ghcPkgOverrides).overrideAttrs ghcDrvOverrides;
      compiler.ghc861 = (ps.haskell.compiler.ghc861.override ghcPkgOverrides).overrideAttrs ghcDrvOverrides;
      compiler.ghc862 = (ps.haskell.compiler.ghc862.override ghcPkgOverrides).overrideAttrs ghcDrvOverrides;
      compiler.ghc863 = (ps.haskell.compiler.ghc863.override ghcPkgOverrides).overrideAttrs ghcDrvOverrides;
    };
  };
}
