# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.

{ localLib ? import ./lib.nix }:

let
  shell = (import ./default.nix {}).shell;
  pkgs = localLib.pkgs;
  devops = pkgs.stdenv.mkDerivation {
    name = "devops-shell";
    buildInputs = [
      localLib.niv
    ];
    shellHook = ''
      echo "DevOps Tools" \
      | ${pkgs.figlet}/bin/figlet -f banner -c \
      | ${pkgs.lolcat}/bin/lolcat

      echo "NOTE: you may need to export GITHUB_TOKEN if you hit rate limits with niv"
      echo "Commands:
        * niv update <package> - update package

      "
    '';
  };

in shell // { inherit devops; }
