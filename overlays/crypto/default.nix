final: prev: {
  libsodium-vrf = final.callPackage ./libsodium.nix {};
  blst = final.callPackage ./blst.nix {};
}
