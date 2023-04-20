final: prev: {
  libsodium-vrf = final.callPackage ./libsodium.nix {};
  blst = final.callPackage ./blst.nix {};
}
# Make these libraries also available to haskell-nix's pkg-config
# map when solving for dependencies with cabal.
// prev.lib.optionalAttrs (prev ? haskell-nix) {
  haskell-nix = prev.haskell-nix // {
    extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings // {
      "libblst" = [ "blst" ];
      # map libsoidum to our libsodium-vrf, if you include the iohk-nix
      # crypto overlay, you _do_ want the custom libsoidum.
      "libsodium" = [ "libsodium-vrf" ];
    };
  };
}
