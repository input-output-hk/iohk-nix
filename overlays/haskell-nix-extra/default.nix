pkgs: super: with pkgs; with lib; {
  haskell-nix = recursiveUpdate super.haskell-nix {
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
    nix-tools = super.haskell-nix.nix-tools.ghc865;
  };

  haskellBuildUtils = pkgs.callPackage ./utils/default.nix {};
}
