# Import and build nix-tools, which is used for generating haskell
# package sets in nix.

{ pkgs }:
let
  src = pkgs.fetchFromGitHub {
    owner = "angerman";
    repo = "nix-tools";
    rev = "d4e4b74d539fee8e580e1531637db67053b62676";
    sha256 = "1y7rm6ds14hii97q34h1mdjfhk0mx2jxiyd295vsnncf3lvcrfn6";
    fetchSubmodules = true;
  };
  nix-tools = import src { inherit pkgs; };
in
  nix-tools.nix-tools-all-execs
