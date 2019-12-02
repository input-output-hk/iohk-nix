let
  commonLib = import ./. {};
  pkgs = commonLib.pkgs;
  shell = pkgs.stdenv.mkDerivation {
    name = "shell";
    buildInputs = [ commonLib.niv ];
    shellHook = ''
      echo "IOHK NIX" \
      | ${pkgs.figlet}/bin/figlet -f banner -c \
      | ${pkgs.lolcat}/bin/lolcat

      echo "Run niv update <package> to update to latest"
      echo "Run niv update to update all packages"
      echo "NOTE: you may exceed github requests"
      echo "This can be resolved with:"
      echo "    export GITHUB_TOKEN=<YOUR_TOKEN>"
    '';
  };
in shell
