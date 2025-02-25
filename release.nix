let commonLib = (import ./. {}); in
{ system ? builtins.currentSystem
, config ? {}
, pkgs ? commonLib.getPkgs { inherit system config; }

# this is passed in by hydra to provide us with the revision
, iohk-nix ? { outPath = ./.; rev = "abcdef"; }

, scrubJobs ? true
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, nixpkgsArgs ? {
    config = config // { allowUnfree = false; inHydra = true; };
  }
}:

with (import (commonLib.nixpkgs + "/pkgs/top-level/release-lib.nix") {
  inherit supportedSystems scrubJobs nixpkgsArgs;
  packageSet = import ./.;
});

with pkgs.lib;

let
  inherit (commonLib) jormungandrLib cardanoLib;

  jormungandrPackages = foldl' (sum: name:
    recursiveUpdate {
      jormungandrLib.environments.${name} = {
        packages = {
          jcli = supportedSystems;
          jcli-debug = supportedSystems;
          jormungandr = supportedSystems;
          jormungandr-debug = supportedSystems;
        };
      };
    } sum
  ) {} (attrNames jormungandrLib.environments);

  usedJormungandrVersions = flatten (mapAttrsToList (name: env:
    with env.packages; [ jcli jcli-debug jormungandr jormungandr-debug ]
  ) jormungandrLib.environments);

  jormungandrConfigs = jormungandrLib.forEnvironments jormungandrLib.mkConfigHydra;

  mappedPkgs = mapTestOn ({
    rust-packages.pkgs.cardano-http-bridge = supportedSystems;
    haskell-nix-extra-packages.stackNixRegenerate = supportedSystems;
    haskell-nix-extra-packages.haskellBuildUtils = supportedSystems;

    # Development tools
  } // jormungandrPackages);

in
fix (self: mappedPkgs // {
  inherit (commonLib) check-hydra;
  inherit jormungandrConfigs;
  jormungandr-deployment = jormungandrLib.mkConfigHtml { inherit (jormungandrLib.environments) itn_rewards_v1 beta nightly legacy; };
  cardano-deployment = cardanoLib.mkConfigHtml { inherit (cardanoLib.environments) mainnet preprod preview; };

  forceNewEval = pkgs.writeText "forceNewEval" iohk-nix.rev;
  required = pkgs.lib.hydraJob (pkgs.releaseTools.aggregate {
    name = "required";
    constituents = (with self; [
      self.forceNewEval
      rust-packages.pkgs.cardano-http-bridge.x86_64-linux
      haskell-nix-extra-packages.stackNixRegenerate.x86_64-linux
      haskell-nix-extra-packages.haskellBuildUtils.x86_64-linux
    ]) ++ usedJormungandrVersions;
  });
})
