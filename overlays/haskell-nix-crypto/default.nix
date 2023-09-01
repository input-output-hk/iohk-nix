# /!\ This overlay depends on both the iohk-nix crypto and the haskell.nix overlays in prev,
# so must be after them in the list of overlays to nixpkgs.
final: prev: {
  # Make libraries from the crypto overlays available to
  # haskell-nix's pkg-config map when solving for dependencies with cabal.
  haskell-nix = prev.haskell-nix or {} // {
    extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings or {} // {
      "libblst" = [ "libblst" ];
      # map libsoidum to our libsodium-vrf, if you include the iohk-nix
      # crypto overlay, you _do_ want the custom libsoidum.
      "libsodium" = [ "libsodium-vrf" ];
      # for secp256k1, haskell.nix already has that mapping, thus we don't
      # need to inject anything extra here.
    };
  };
}
