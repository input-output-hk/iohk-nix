############################################################################
# iohk-skeleton Nix build
#
# fixme: document top-level attributes and how to build them
#
############################################################################

{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize ghc and profiling (see ./nix/haskell.nix):
, config ? {}
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of nixpkgs and iohk-nix:
# nix build -f default.nix iohk-skeleton --arg sourcesOverride '{
#   iohk-nix = ../iohk-nix;
#   nixpkgs  = ../nixpkgs;
# }'
, sourcesOverride ? {}
# pinned version of nixpkgs augmented with iohk overlays.
, pkgs ? import ./nix {
    inherit system crossSystem config sourcesOverride;
  }
}:
# commonLib include iohk-nix utilities, our util.nix and nixpkgs lib.
with pkgs; with commonLib;
let


  haskellPackages = recRecurseIntoAttrs
    # the Haskell.nix package set, reduced to local packages.
    (selectProjectPackages skeletonHaskellPackages);

  self = {
    inherit haskellPackages check-hydra;

    inherit (haskellPackages.iohk-skeleton.identifier) version;
    # Grab the executable component of our package.
    inherit (haskellPackages.iohk-skeleton.components.exes)
      iohk-skeleton;

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" haskellPackages;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks haskellPackages;
      # Example of a linting script used by Buildkite.
      lint-fuzz = callPackage ./nix/check-lint-fuzz.nix {};
    };

    shell = import ./shell.nix {
      inherit pkgs;
      withHoogle = true;
    };

    # Attrset of PDF builds of LaTeX documentation.
    docs = pkgs.callPackage ./docs/default.nix {};
  };
in
  self
