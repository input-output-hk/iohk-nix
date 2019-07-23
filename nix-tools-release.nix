commonLib: # the iohk-nix commonLib
{ packages ? []
, required-name ? "required"
, required-targets ? (jobsets: [])
, extraBuilds ? {}
, builds-on-supported-systems ? []
, config ? {}
# information hydra passes in about the current
# package under evaluation.
, _this ? { outPath = ./.; rev = "abcdef"; }
, package-set-path # usually ./.
# all derivations with a path that starts with one of the values in `disabled-jobs` is ignored:
, disabled-jobs ? []
}:
{ system ? builtins.currentSystem
, pkgs ? commonLib.getPkgs { inherit system config; }

, scrubJobs ? true
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, nixpkgsArgs ? {
    config = config // { allowUnfree = false; inHydra = true; };
  }
}:
with (import (commonLib.nixpkgs + "/pkgs/top-level/release-lib.nix") {
  inherit supportedSystems scrubJobs nixpkgsArgs;
  packageSet = import package-set-path;
});
with pkgs.lib;
let

  traceId = x: builtins.trace (builtins.deepSeq x x) x;

  # we only pass an empty argument set {} as a dummy here as
  # we are interested in extracting the nix-tools node and
  # generate the necessary input for the mapTestOn / mapTestOnCross
  # calls here.
  packageSet = import package-set-path {};
  nix-tools-pkgs = supportedSystems: {
    nix-tools.libs =
      mapAttrs (_: _: supportedSystems)
        (filterAttrs (n: v: builtins.elem n packages && v != null) packageSet.nix-tools.libs);
    # aggreated exes
    nix-tools.exes =
      mapAttrs (_: _: supportedSystems)
        (filterAttrs (n: v: builtins.elem n packages && v != null) packageSet.nix-tools.exes);
    # component exes exposed
    nix-tools.cexes =
      mapAttrs (_: mapAttrs (_: _: supportedSystems))
        (filterAttrs (n: v: builtins.elem n packages && v != null) packageSet.nix-tools.cexes);
    nix-tools.tests =
      mapAttrs (_: mapAttrs (_: _: supportedSystems))
        (filterAttrs (n: v: builtins.elem n packages && v != null) packageSet.nix-tools.tests);
    nix-tools.benchmarks =
      mapAttrs (_: mapAttrs (_: _: supportedSystems))
        (filterAttrs (n: v: builtins.elem n packages && v != null) packageSet.nix-tools.benchmarks);
  };

  mapped-pkgs = mapTestOn (nix-tools-pkgs supportedSystems);

  mapped-builds-on-supported-systems = mapTestOn (listToAttrs 
    (map (n: nameValuePair n supportedSystems) builds-on-supported-systems));

  # we use builtins.currentSystem here as that will evaluate to whatever the evaluator runs on.
  # thus someone on macOS will be able to build the .x86_64-darwin cross expressions, while
  # someone on linux will be able to build the .x86_64-linux ones.  As hydra is running on
  # linux, this should also only present CI with the .x86_64-linux targets.  This currently only
  # applies to mingw32 as that is our only cross compliation target for now.  We may later
  # add muslc/ghcjs/wasm, and other targets as needed.
  mapped-pkgs-mingw32 = mapTestOnCross lib.systems.examples.mingwW64 (nix-tools-pkgs [ builtins.currentSystem ]);
  mapped-pkgs-mingw32-renamed =
        (lib.mapAttrs (_: (lib.mapAttrs (_: (lib.mapAttrs' (n: v: lib.nameValuePair (lib.systems.examples.mingwW64.config + "-" + n) v)))))
          mapped-pkgs-mingw32);
  mapped-pkgs-partial
    = lib.recursiveUpdate
        mapped-pkgs
        mapped-pkgs-mingw32-renamed;
  mapped-pkgs-all
    = lib.recursiveUpdate
        (lib.recursiveUpdate mapped-pkgs-partial extraBuilds)
        mapped-builds-on-supported-systems;

  filtered-pkgs-all = lib.mapAttrsRecursiveCond
    (as: !(as ? "type" && as.type == "derivation"))
    (path: v: 
      let dottedPath = builtins.concatStringsSep "." path; 
      in if builtins.any (d: lib.hasPrefix d dottedPath) disabled-jobs then null else v)
    mapped-pkgs-all;

  aggregate-pkgs = let
    w64 = lib.systems.examples.mingwW64.config;
    cS = builtins.currentSystem;
  in foldr (cType: (lib.recursiveUpdate {
    # we define aggregate jobs that respectively gather all libs, all exes, all tests and all benchmarks
    # in the `packages` list.
    nix-tools."packages-${cType}" = listToAttrs (map (s: nameValuePair s (pkgs.lib.hydraJob (pkgs.releaseTools.aggregate {
      name = "packages-${cType}.${s}";
      constituents = filter (d: d.system == s) (collect isDerivation
        (filterAttrs (n: v: builtins.elem n packages && v != null) filtered-pkgs-all.nix-tools."${cType}"));
    }))) supportedSystems);
    # corresponding Windows64 cross expression:
    nix-tools."${w64}-packages-${cType}"."${cS}" = pkgs.lib.hydraJob (pkgs.releaseTools.aggregate {
      name = "${w64}-packages-${cType}.${cS}";
      constituents = filter (d: d.system == cS) (collect isDerivation
        (filterAttrs (n: v: builtins.elem n (map (p: "${w64}-${p}") packages) && v != null) filtered-pkgs-all.nix-tools."${cType}"));
    });
  })) {} ["libs" "exes" "tests" "benchmarks"];

  pkgs-all
    = lib.recursiveUpdate
        filtered-pkgs-all
        aggregate-pkgs;

in fix (self: (builtins.removeAttrs packageSet ["nix-tools" "_lib"]) // pkgs-all
// {
  forceNewEval = pkgs.writeText "forceNewEval" _this.rev;
  required = pkgs.lib.hydraJob (pkgs.releaseTools.aggregate {
    name = required-name;
    constituents = [ self.forceNewEval ] ++ required-targets self;
  });

})
