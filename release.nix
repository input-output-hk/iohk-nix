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
  packageSet = import ./. {};
  inherit (packageSet) jormungandrLib;

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
    niv = supportedSystems;

    # Development tools
  } // jormungandrPackages);

  skeletonJobset = import ./skeleton/release.nix {
    sourcesOverride = { iohk-nix = ./.; };
  };

  mkPins = inputs: pkgs.runCommand "ifd-pins" {} ''
    mkdir $out
    cd $out
    ${concatMapStringsSep "\n" (input: "ln -sv ${input.value} ${input.key}") (attrValues (mapAttrs (key: value: { inherit key value; }) inputs))}
  '';

in
fix (self: mappedPkgs // {
  inherit (commonLib) check-hydra;
  inherit jormungandrConfigs;
  jormungandr-deployment = jormungandrLib.mkConfigHtml { inherit (jormungandrLib.environments) itn_rewards_v1 beta nightly legacy; };

  ifd-pins = mkPins {
    # todo, build it for both linux and darwin
    iohk-nix-utils-cabal2nix = (packageSet.pkgs.extend (builtins.elemAt packageSet.overlays.haskell-nix-extra 0)).haskellBuildUtils.package.cabal2nixDeriver;
  };

  forceNewEval = pkgs.writeText "forceNewEval" iohk-nix.rev;
  required = pkgs.lib.hydraJob (pkgs.releaseTools.aggregate {
    name = "required";
    constituents = (with self; [
      self.forceNewEval
      rust-packages.pkgs.cardano-http-bridge.x86_64-linux
      niv.x86_64-linux
      niv.x86_64-darwin
    ]) ++ usedJormungandrVersions;
  });
} // {
  skeleton = skeletonJobset;
})
