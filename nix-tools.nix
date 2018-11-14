# Import and build nix-tools, which is used for generating haskell
# package sets in nix.

{ pkgs }:
let
  src = pkgs.fetchFromGitHub {
    owner = "angerman";
    repo = "nix-tools";
    rev = "9b1066f5613b35037231cf2dbb6db25cb60c4205";
    sha256 = "0h03dv1g2ykdhdkvbzlwfyykwmvf5pqw4672x7mbx8vzm1i0s1q0";
  };
  nix-tools = import src { inherit pkgs; };
in
  nix-tools.nix-tools-all-execs
