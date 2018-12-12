# Import and build nix-tools, which is used for generating haskell
# package sets in nix.

{ pkgs }:
let
  src = pkgs.fetchFromGitHub {
    owner = "angerman";
    repo = "nix-tools";
    rev = "b7835666bcf73c3fa50f5c59bfa4cac29ea8d626";
    sha256 = "1pnw10jvlb6gld6ja294yzf4mlnc8a0sxml3d0mhgi7ns4zr9ggy";
    fetchSubmodules = true;
  };
  nix-tools = import src { inherit pkgs; };
in
  nix-tools.nix-tools-all-execs
