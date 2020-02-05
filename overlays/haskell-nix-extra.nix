pkgs: super: {
  haskell-nix = pkgs.lib.recursiveUpdate super.haskell-nix {
    haskellLib.extra = with pkgs.haskell-nix.haskellLib; {

      collectChecks = pkgsSet: (pkgs.lib.mapAttrs (_: package: package.checks) pkgsSet)
         // { recurseForDerivations = true; };

      collectComponents' = group: collectComponents group (_:true);

    };
  };
}
