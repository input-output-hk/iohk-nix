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
          jormungandr = supportedSystems;
        };
      };
    } sum
  ) {} (attrNames jormungandrLib.environments);

  usedJormungandrVersions = flatten (mapAttrsToList (name: env:
    with env.packages; [ jcli jormungandr ]
  ) jormungandrLib.environments);

  jormungandrConfigs = jormungandrLib.forEnvironments jormungandrLib.mkConfigHydra;

  mappedPkgs = mapTestOn ({
    nix-tools.package            = supportedSystems;
    nix-tools.regeneratePackages = supportedSystems;
    rust-packages.pkgs.cardano-http-bridge = supportedSystems;
    niv = supportedSystems;

    # this seems not to work :-/
    # tests.hlint                  = supportedSystems;
    # tests.shellcheck             = supportedSystems;
    # tests.stylishHaskell         = supportedSystems;

    # Development tools
    cache-s3 = supportedSystems;
    stack-hpc-coveralls = supportedSystems;
    openapi-spec-validator = supportedSystems;
  } // jormungandrPackages);

  skeletonJobset = import ./skeleton/release.nix {
    iohkLib = packageSet;
  };

in
fix (self: mappedPkgs // {
  inherit (commonLib) check-hydra;
  inherit jormungandrConfigs;
  jormungandr-deployment = jormungandrLib.mkConfigHtml;

  forceNewEval = pkgs.writeText "forceNewEval" iohk-nix.rev;
  required = pkgs.lib.hydraJob (pkgs.releaseTools.aggregate {
    name = "required";
    constituents = (with self; [
      self.forceNewEval
      nix-tools.package.x86_64-linux
      nix-tools.package.x86_64-darwin
      nix-tools.regeneratePackages.x86_64-linux
      nix-tools.regeneratePackages.x86_64-darwin
      rust-packages.pkgs.cardano-http-bridge.x86_64-linux
      niv.x86_64-linux
      niv.x86_64-darwin
    ]) ++ usedJormungandrVersions;
  });
} // {
  skeleton = skeletonJobset;
})
