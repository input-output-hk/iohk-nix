final: prev: {
  libsodium-vrf = final.callPackage ./libsodium.nix {};
  blst = final.callPackage ./blst.nix {};
}
# Make these libraries also available to haskell-nix's pkg-config
# map when solving for dependencies with cabal.
// final.lib.optionalAttrs (prev ? haskell-nix) {
  haskell-nix = prev.haskell-nix // {
    extraPkgconfigMappings = super.haskell-nix.extraPkgconfigMappings // {
      "libblst" = [ "blst" ];
      "libsodium-vrf" = [ "libsodium-vrf" ];
    };
  };
}
