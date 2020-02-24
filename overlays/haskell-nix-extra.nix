pkgs: super: with pkgs; with lib; {
  haskell-nix = recursiveUpdate super.haskell-nix {
    haskellLib.extra = with haskell-nix.haskellLib; rec {

      collectChecks = pkgsSet: (mapAttrs (_: package: package.checks) pkgsSet)
         // { recurseForDerivations = true; };

      recRecurseIntoAttrs = x:
        if (isAttrs x && !isDerivation x)
        then recurseIntoAttrs (mapAttrs (n: v: if n == "buildPackages" then v else recRecurseIntoAttrs v) x)
        else x;

    };
  };
  stackNixRegenerate = pkgs.callPackage ./haskell-nix-extra/nix-tools-regenerate.nix {
    nix-tools = super.haskell-nix.nix-tools;
  };
  haskellBuildUtils = import ./haskell-nix-extra/utils/default.nix {
    pkgs = super;
  };
  stack-hpc-coveralls = super.haskellPackages.callPackage ./stack-hpc-coveralls.nix {};
}
