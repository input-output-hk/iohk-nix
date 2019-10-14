{ lib, runCommand, fetchFromGitHub, haskell }:

let
  src = fetchFromGitHub {
    owner = "jaspervdj";
    repo = "stylish-haskell";
    rev = "a5d8cd34bcde3ad4698bbe38c9f14f7a1074ef4c";
    sha256 = "1rrwp3g0ad0viismd1cs55jh9xpnjnadllmwxvg42cypg8axca2q";
  };

  pkgSet = haskell.mkStackPkgSet {
    stack-pkgs = (haskell.importAndFilterProject (haskell.callStackToNix {
      inherit src;
    })).pkgs;
    pkg-def-extras = [];
    modules = [];
  };

  packages = pkgSet.config.hsPkgs;

  inherit (packages.stylish-haskell.components.exes) stylish-haskell;

in
  stylish-haskell
