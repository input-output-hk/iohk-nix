{ sources ? import ./sources.nix
, system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
}:
import sources.nixpkgs {
  inherit system crossSystem config;
  overlays = [ overlay ];
}
