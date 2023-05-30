inputs: final: prev: {
  # We pin our own crypto libraries here, so that we have control over
  # the specific revisions we use (assuming a recent enough iohk-nix).
  #
  #  This allows us to set the sepcific version/rev we want in the
  #  flake.nix.  We _do not_ pin the generic libsodium, and rely on
  #  the upstream one provided by what ever nixpkgs pin is used.
  #
  libsodium-vrf = final.callPackage ./libsodium.nix { src = inputs.sodium; };
  libblst = final.callPackage ./libblst.nix { src = inputs.blst; };
  # we will follow nixpkg upstreams naming scheme where needed. We want to pin
  # this library, but also don't start dealing with multiple libraries with
  # different but same names, and then risk that we accidentally diverge.
  secp256k1 = final.callPackage ./libsecp256k1.nix { src = inputs.secp256k1; };
}
