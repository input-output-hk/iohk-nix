{ sources ? import ./sources.nix
, system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
}:
let
  overlay = final: prev: {
    inherit (import sources.nixpkgs-crystal {}) crystal;
  };
in import sources.nixpkgs {
  inherit system crossSystem config;
  overlays = [ overlay ];
}
