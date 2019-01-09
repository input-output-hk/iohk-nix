# Import and build nix-tools, which is used for generating haskell
# package sets in nix.

{ pkgs }:
let
  src = pkgs.fetchFromGitHub {
    owner = "angerman";
    repo = "nix-tools";
    rev = "11186ed3b8109b98b38bcd0e33a9530e59868510";
    sha256 = "1s26nk24iqzbmzixwv7sgm982kbzqgprn5snrhzzxya2zvyhzp4i";
  };
  nix-tools = import src { inherit pkgs; };
in
  nix-tools.nix-tools-all-execs
