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
  ghc861 = ghc86;
  ghc862 = ghc86;
  ghc863 = ghc86;
  ghc864 = ghc86;
}
