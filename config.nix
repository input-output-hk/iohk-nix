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
    ghcDrvOverrides = drv: y{
        dontStrip = true;
        hardeningDisable = (drv.hardeningDisable or []) ++ [ "stackprotector" "format" ];
        patches = (drv.patches or [])
         # Patches for which we know they have been merged into a public release already
         ++ lib.optional (builtins.compareVersions drv.version "8.4.3" == 1
                       && builtins.compareVersions drv.version "8.6" == -1)   ./patches/ghc/ghc-8.4.4-reinstallable-lib-ghc.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.6" == -1)   ./patches/ghc/move-iserv-8.4.2.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.6" == -1)   ./patches/ghc/hsc2hs-8.4.2.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.6" == -1)   ./patches/ghc/various-8.4.2.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.6" == -1)   ./patches/ghc/lowercase-8.4.2.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.6" == -1)   ./patches/ghc/cabal-exe-ext-8.4.2.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.6" == -1)   ./patches/ghc/ghc-8.4.3-Cabal2201-SMP-test-fix.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.6" == -1)   ./patches/ghc/outputtable-assert-8.4.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.5" ==  1
                       && builtins.compareVersions drv.version "8.6.4" == -1) ./patches/ghc/MR148--T16104-GhcPlugins.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.6.4" == -1) ./patches/ghc/MR95--ghc-pkg-deadlock-fix.patch

         # Patches for which we only know a lower bound.
         ++ lib.optional (builtins.compareVersions drv.version "8.5" ==  1)
          ./patches/ghc/iserv-proxy-cleanup.patch                             # https://gitlab.haskell.org/ghc/ghc/merge_requests/250  -- merged; ghc-8.8.1
         ++ lib.optional (builtins.compareVersions drv.version "8.2" ==  1)
          ./patches/ghc/MR545--ghc-pkg-databases.patch                        # https://gitlab.haskell.org/ghc/ghc/merge_requests/545  -- merged; ghc-8.8.1
         ++ lib.optional (builtins.compareVersions drv.version "8.5" ==  1)
          ./patches/ghc/outputtable-assert-8.6.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.5" ==  1)
          ./patches/ghc/mistuke-ghc-err_clean_up_error_handler-8ab1a89af89848f1713e6849f189de66c0ed7898.diff # this is part of Phyx- revamped io-manager.
         ++ [
          ./patches/ghc/ghc-add-keepCAFs-to-rts.patch                         # https://gitlab.haskell.org/ghc/ghc/merge_requests/950  -- open
          ./patches/ghc/lowercase-8.6.patch                                   # https://gitlab.haskell.org/ghc/ghc/merge_requests/949  -- merged; ghc-8.8.1
          ./patches/ghc/dll-loader-8.4.2.patch                                # https://gitlab.haskell.org/ghc/ghc/merge_requests/949  -- open
          ./patches/ghc/0001-Stop-the-linker-panic.patch                      # https://phabricator.haskell.org/D5012                  -- merged; ghc-8.8.1
          ./patches/ghc/ghc-8.4.3-Cabal2201-no-hackage-tests.patch            # ?
          ./patches/ghc/ghc-8.4.3-Cabal2201-allow-test-wrapper.patch          # https://github.com/haskell/cabal/pulls/5995            -- merged; cabal-3.0.0 (ghc-8.8.1)
          ./patches/ghc/ghc-8.4.3-Cabal2201-response-file-support.patch       # https://github.com/haskell/cabal/pulls/5996            -- merged; cabal-3.0.0 (ghc-8.8.1)
          ./patches/ghc/ghc-8.6-Cabal-fix-datadir.patch                       # https://github.com/haskell/cabal/issues/5862
          ./patches/ghc/MR196--ghc-pkg-shut-up.patch                          # https://gitlab.haskell.org/ghc/ghc/merge_requests/196  -- merged; ghc-8.8.1
          ./patches/ghc/MR948--32bit-cross-th.patch                           # https://gitlab.haskell.org/ghc/ghc/merge_requests/948  -- open
         ]

         # Patches for specific ghc versions.
         ++ lib.optional (builtins.compareVersions drv.version "8.6.3" == 0)  ./patches/ghc/T16057--ghci-doa-on-windows.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.6.3" == 0)  ./patches/ghc/ghc-8.6.3-reinstallable-lib-ghc.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.6.4" == 0)  ./patches/ghc/ghc-8.6.4-reinstallable-lib-ghc.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.6.5" == 0)  ./patches/ghc/ghc-8.6.5-reinstallable-lib-ghc.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.6.4" == 0)  ./patches/ghc/ghc-8.6.4-reenable-th-qq-in-stage1.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.6.5" == 0)  ./patches/ghc/ghc-8.6.4-reenable-th-qq-in-stage1.patch
         ++ lib.optional (builtins.compareVersions drv.version "8.6.4" == 0)  ./patches/ghc/ghc-8.6.4-better-plusSimplCountErrors.patch
         ;
        postPatch = (drv.postPatch or "") + "\n" + "autoreconf";
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
       compiler = lib.mapAttrs (_name: compiler: (compiler.override ghcPkgOverrides).overrideAttrs ghcDrvOverrides)
         # these patches (ghcPkgOverrides and ghcDrvOverrides) only apply to vanilla source ghcs. Not ghcjs or binary distributions.
         # we also ignore ghc82. And are only concerned with ghc84+
         (lib.filterAttrs
           (n: _value: !(ps.stdenv.targetPlatform.isGhcjs or false) # we want to apply this only to non-ghcjs ones. As we do some ghc <- ghcjs mapping for ghcjs.
                       && lib.hasPrefix "ghc" n && !lib.hasPrefix "ghc82" n && !lib.hasPrefix "ghcjs" n && !lib.hasSuffix "Binary" n) ps.haskell.compiler);
       };
  };
}
