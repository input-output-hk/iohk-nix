inputs: final: prev: rec {
  # We pin our own crypto libraries here, so that we have control over
  # the specific revisions we use (assuming a recent enough iohk-nix).
  #
  #  This allows us to set the sepcific version/rev we want in the
  #  flake.nix.  We _do not_ pin the generic libsodium, and rely on
  #  the upstream one provided by what ever nixpkgs pin is used.
  #
  libsodium-vrf = final.callPackage ./libsodium.nix { inherit inputs; };
  libblst = final.callPackage ./libblst.nix { inherit inputs; };
  libsecp256k1 = final.callPackage ./libsecp256k1.nix { inherit inputs; };

  # override the nixpkgs ones which do not have the `lib` prefix.
  secp256k1 = libsecp256k1;
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
