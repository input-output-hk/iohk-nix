commonLib: # the iohk-nix commonLib
{ packages ? []
, required-name ? "required"
, required-targets ? (jobsets: [])
, config ? {}
, rev ? "abcdef"
, package-set-path # usually import ./. {}
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

  packageSet = import package-set-path {};
  nix-tools-pkgs = supportedSystems: {
    nix-tools.libs =
      mapAttrs (_: _: supportedSystems)
        (filterAttrs (n: v: builtins.elem n packages && v != null) packageSet.nix-tools.libs);
    nix-tools.exes =
      mapAttrs (_: mapAttrs (_: _: supportedSystems))
        (filterAttrs (n: v: builtins.elem n packages && v != null) packageSet.nix-tools.exes);
    nix-tools.tests =
      mapAttrs (_: mapAttrs (_: _: supportedSystems))
        (filterAttrs (n: v: builtins.elem n packages && v != null) packageSet.nix-tools.tests);
    nix-tools.benchmarks =
      mapAttrs (_: mapAttrs (_: _: supportedSystems))
        (filterAttrs (n: v: builtins.elem n packages && v != null) packageSet.nix-tools.benchmarks);
  };

  mapped-pkgs = mapTestOn (nix-tools-pkgs supportedSystems);
  # we use builtins.currentSystem here as that will evaluate to whatever the evaluator runs on.
  # thus someone on macOS will be able to build the .x86_64-darwin cross expressions, while
  # someone on linux will be able to build the .x86_64-linux ones.  As hydra is running on
  # linux, this should also only present CI with the .x86_64-linux targets.
  mapped-pkgs-mingw32 = mapTestOnCross lib.systems.examples.mingwW64 (nix-tools-pkgs builtins.currentSystem);

  mapped-pkgs-all
    = lib.recursiveUpdate
        (mapped-pkgs)
        (lib.mapAttrs (_: (lib.mapAttrs (_: (lib.mapAttrs' (n: v: lib.nameValuePair (lib.systems.examples.mingwW64.config + "-" + n) v)))))
          mapped-pkgs-mingw32);

in fix (self: (builtins.removeAttrs packageSet ["nix-tools" "_lib"]) // mapped-pkgs-all
// {
  forceNewEval = pkgs.writeText "forceNewEval" rev;
  required = pkgs.lib.hydraJob (pkgs.releaseTools.aggregate {
    name = required-name;
    constituents = [ forceNewEval ] ++ required-targets self;
  });

})
