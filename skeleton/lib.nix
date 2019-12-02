let
  sources = import ../nix/sources.nix;
  pkgs' = import sources.nixpkgs {};
  # changes to sources.iohk-nix in your project
  iohkNix = import ../. { };
  pkgs = iohkNix.pkgs;
  lib = pkgs.lib;
  niv = (import sources.niv {}).niv;
in lib // iohkNix // {
  inherit iohkNix pkgs niv;
}
