{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, sourcesOverride ? {}
}:
let
  # use default stable nixpkgs from iohk-nix instead of our own:
  sources = removeAttrs (import ./sources.nix) [ "nixpkgs" ] //
    {
      # alternatively, use iohk-nix default unstable nixpkgs:
      #nixpkgs = iohkNix.sources.nixpkgs-unstable;
    } //
    sourcesOverride;

  # for inclusion in pkgs:
  nixpkgsOverlays = [
    (_: _: { commonLib = lib // iohkNix; })
  ];

  # Import IOHK common nix lib, using our sources as override:
  iohkNix = import sources.iohk-nix {
    inherit system crossSystem config nixpkgsOverlays;
    sourcesOverride = sources;
  };
  pkgs = iohkNix.pkgs;
  lib = pkgs.lib;
in
  pkgs
