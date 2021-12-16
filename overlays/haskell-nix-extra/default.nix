let
  compiler-nix-name = "ghc8107";
  index-state = "2021-12-13T00:00:00Z";
in final: prev: with final; with lib; {
  haskell-nix = recursiveUpdate prev.haskell-nix {
    # TODO: remove this haskellLib.extra
    haskellLib.extra = rec {
      collectChecks =
        trace ( "Warning: `haskell-nix.haskellLib.extra.collectChecks`"
              + " is deprecated and will be removed. Please use"
              + " `haskell-nix.haskellLib.collectChecks'`.")
        haskell-nix.haskellLib.collectChecks';

      recRecurseIntoAttrs = x:
        if (isAttrs x && !isDerivation x && x.recurseForDerivations or true)
        then recurseIntoAttrs (mapAttrs (n: v: if n == "buildPackages" then v else recRecurseIntoAttrs v) x)
        else x;

    };
  };

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
}
