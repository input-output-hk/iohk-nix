pkgs: super: with pkgs; with lib; {
  haskell-nix = recursiveUpdate super.haskell-nix {
    haskellLib.extra = with haskell-nix.haskellLib; rec {

      collectChecks = pkgsSet: (mapAttrs (_: package: package.checks) pkgsSet)
         // { recurseForDerivations = true; };

      collectComponents' = group: collectComponents group (_:true);

      recRecurseIntoAttrs = x:
        if (isAttrs x && !isDerivation x)
        then recurseIntoAttrs (mapAttrs (n: v: if n == "buildPackages" then v else recRecurseIntoAttrs v) x)
        else x;

    };
  };
}
