final: prev: {
  libsodium-vrf = final.callPackage ./libsodium.nix {};
  libblst = final.callPackage ./libblst.nix {};
}
# Make these libraries also available to haskell-nix's pkg-config
# map when solving for dependencies with cabal.
// prev.lib.optionalAttrs (prev ? haskell-nix) {
  haskell-nix = prev.haskell-nix // {
    extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings // {
      "libblst" = [ "libblst" ];
      # map libsoidum to our libsodium-vrf, if you include the iohk-nix
      # crypto overlay, you _do_ want the custom libsoidum.
      "libsodium" = [ "libsodium-vrf" ];
    };
  };
}
