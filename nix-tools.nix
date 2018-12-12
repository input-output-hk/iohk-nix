# Import and build nix-tools, which is used for generating haskell
# package sets in nix.

{ pkgs }:
let
  src = pkgs.fetchFromGitHub {
    owner = "angerman";
    repo = "nix-tools";
    rev = "35c161984661667a44f92c57b1d7df7222ce0e86";
    sha256 = "0bw9w733viacf32izr70rvmmrrdsjhjw7yi0hgwbjzwzilnhrks6";
    fetchSubmodules = true;
  };
  nix-tools = import src { inherit pkgs; };
in
  nix-tools.nix-tools-all-execs
