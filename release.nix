let commonLib = (import ./. {}); in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? commonLib.getPkgs { inherit system config; }

, scrubJobs ? true
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, nixpkgsArgs ? {
    config = config // { allowUnfree = false; inHydra = true; };
  }
}:
with (import (commonLib.nixpkgs + "/pkgs/top-level/release-lib.nix") {
  inherit supportedSystems scrubJobs nixpkgsArgs;
  packageSet = import ./.;
}); with pkgs.lib;
let
 packageSet = import ./. {};
 mappedPkgs = mapTestOn {
    nix-tools.package            = supportedSystems;
    nix-tools.regeneratePackages = supportedSystems;

    # this seems not to work :-/
    # tests.hlint                  = supportedSystems;
    # tests.shellcheck             = supportedSystems;
    # tests.stylishHaskell         = supportedSystems;
 };
in
fix (self: mappedPkgs // {
  required = pkgs.lib.hydraJob (pkgs.releaseTools.aggregate {
    name = "required";
    constituents = with self; [
      nix-tools.package.x86_64-linux
      nix-tools.package.x86_64-darwin
      nix-tools.regeneratePackages.x86_64-linux
      nix-tools.regeneratePackages.x86_64-darwin
    ];
  });
})
