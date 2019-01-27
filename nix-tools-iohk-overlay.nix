commonLib:
let
  ghc84 = {};
  ghc86 = hackage: {
    hsc2hs = hackage.hsc2hs."0.68.4".revisions.default;
    # stackage beautifully omitts the Win32 pkg
    Win32 = hackage.Win32."2.6.2.0".revisions.default;
  };
in {
  ghc842 = ghc84;
  ghc843 = ghc84;
  ghc844 = ghc84;
  ghc861 = h: ghc86 h // {
    ghci         = ./ghc-packages/ghci-8.6.1.nix;
    ghc-boot     = ./ghc-packages/ghc-boot-8.6.1.nix;
    libiserv     = ./ghc-packages/libiserv-8.6.1.nix;
    remote-iserv = ./ghc-packages/remote-iserv-8.6.1.nix;
    iserv-proxy  = ./ghc-packages/iserv-proxy-8.6.1.nix;
  };
  ghc862 = h: ghc86 h // {
    ghci         = ./ghc-packages/ghci-8.6.2.nix;
    ghc-boot     = ./ghc-packages/ghc-boot-8.6.2.nix;
    libiserv     = ./ghc-packages/libiserv-8.6.2.nix;
    remote-iserv = ./ghc-packages/remote-iserv-8.6.2.nix;
    iserv-proxy  = ./ghc-packages/iserv-proxy-8.6.2.nix;
  };
  ghc863 = h: ghc86 h // {
    ghci         = ./ghc-packages/ghci-8.6.3.nix;
    ghc-boot     = ./ghc-packages/ghc-boot-8.6.3.nix;
    libiserv     = ./ghc-packages/libiserv-8.6.3.nix;
    remote-iserv = ./ghc-packages/remote-iserv-8.6.3.nix;
    iserv-proxy  = ./ghc-packages/iserv-proxy-8.6.3.nix;
  };
  ghc864 = ghc86;
}
