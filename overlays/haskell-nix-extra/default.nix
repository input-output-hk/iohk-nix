let
  compiler-nix-name = "ghc8107";
  index-state = "2021-12-13T00:00:00Z";
in final: prev: with final; with lib; {

  stackNixRegenerate = pkgs.callPackage ./nix-tools-regenerate.nix {
    nix-tools = haskell-nix.nix-tools.${compiler-nix-name};
  };

  haskellBuildUtils = pkgs.callPackage ./utils/default.nix {
    inherit compiler-nix-name index-state;
  };

  rewriteStatic = _: p: if (pkgs.stdenv.hostPlatform.isDarwin) then
    pkgs.runCommandCC p.name {
      nativeBuildInputs = [ pkgs.haskellBuildUtils pkgs.buildPackages.binutils pkgs.buildPackages.nix ];
    } ''
      cp -R ${p} $out
      chmod -R +w $out
      rewrite-libs $out/bin $out/bin/*
    '' else if (pkgs.stdenv.hostPlatform.isMusl) then
    pkgs.runCommandCC p.name { } ''
      cp -R ${p} $out
      chmod -R +w $out
      $STRIP $out/bin/*
    '' else p;

  # Given a derivation, stamp its Haskell executable with provided version info (git revision),
  # using `set-git-rev` from the haskellBuildUtils package.
  # The target executable must have use "Data.FileEmbed.dummySpace".
  # Return a new derivation that depends on the original one.
  # This is an alternative to using postInstall for stamping,
  # allowing more caching opportunities and a much lower closure size.
  setGitRev = gitrev: drv:
    let
      # The new drivation will copy over existing attributes for compatibility,
      # except `exePath` that need to point to the new $out path.
      newdrv = buildPackages.runCommand drv.name {
        passthru = drv.passthru // (optionalAttrs (drv.passthru ? exeName) {
          exePath = newdrv + "/bin/${drv.passthru.exeName}";
        });
      } ''
          mkdir -p $out
          # We link rather than copy from original, to save some space/time:
          ln -s ${drv}/* $out/
          rm $out/bin
          mkdir $out/bin
          ln -s ${drv}/bin/* $out/bin/
          rm $out/bin/*${stdenv.hostPlatform.extensions.executable}
          cp --no-preserve=timestamps --recursive ${drv}/bin/*${stdenv.hostPlatform.extensions.executable} $out/bin/
          chmod -R +w $out/bin/*${stdenv.hostPlatform.extensions.executable}
          ${pkgsBuildBuild.haskellBuildUtils}/bin/set-git-rev "${gitrev}" $out/bin/*${stdenv.hostPlatform.extensions.executable}
        '';
      in drv // newdrv;

  # Stamp executables from multiple derivations, identified by path in the attribute set
  # (third arg), with version info (git revision). Eg.:
  # `setGitRevForPaths gitrev [
  #     "cardano-db-sync.components.exes.cardano-db-sync"
  #     "cardano-smash-server.components.exes.cardano-smash-server"
  #     "cardano-db-tool.components.exes.cardano-db-tool"] hsPkgs
  setGitRevForPaths = gitrev: exePaths:
    updateManyAttrsByPath (map (path: {
      path = splitString "." path;
      update = setGitRev gitrev;
    }) exePaths);
}
