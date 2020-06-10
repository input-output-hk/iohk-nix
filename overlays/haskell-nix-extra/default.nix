pkgs: super: with pkgs; with lib; {
  haskell-nix = recursiveUpdate super.haskell-nix {
    haskellLib.extra = with haskell-nix.haskellLib; rec {

      collectChecks = pkgsSet: (mapAttrs (_: package: package.checks) pkgsSet)
         // { recurseForDerivations = true; };

      recRecurseIntoAttrs = x:
        if (isAttrs x && !isDerivation x && x.recurseForDerivations or true)
        then recurseIntoAttrs (mapAttrs (n: v: if n == "buildPackages" then v else recRecurseIntoAttrs v) x)
        else x;

    };
  };
  stackNixRegenerate = pkgs.callPackage ./nix-tools-regenerate.nix {
    nix-tools = super.haskell-nix.nix-tools;
  };
  haskellBuildUtils = import ./utils/default.nix {
    inherit pkgs;
  };
  stack-hpc-coveralls = super.haskellPackages.callPackage ./stack-hpc-coveralls.nix {};
}
