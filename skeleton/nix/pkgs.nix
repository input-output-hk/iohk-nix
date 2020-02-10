pkgs: _: with pkgs; {
  skeletonHaskellPackages = import ./haskell.nix {
    inherit
      config
      lib
      stdenv
      haskell-nix
      buildPackages
      makeWrapper
      jormungandr
      cowsay;
  };
}
