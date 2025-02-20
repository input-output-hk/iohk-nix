{ config ? {}
, system ? builtins.currentSystem
, crossSystem ? null
, sourcesOverride ? {}
# Set application for getting a specific application nixkgs-src.json
, application ? ""
# Override nixpkgs-src.json to a file in your repo
, nixpkgsOverride ? ""
, nixpkgsJsonOverride ? ""
# Modify nixpkgs with overlays
, nixpkgsOverlays ? []
, defaultSources ? import ./nix/sources.nix { pkgs = pkgsDefault; }
, pkgsDefault ? import defaultSources.nixpkgs { inherit config system crossSystem; }
}:

let
  upstreamedDeprecation = p: builtins.trace "WARNING: commonLib.${p} is deprecated. Please use it from nixpkgs directly instead.";
  sources = defaultSources // sourcesOverride;

  commonLib = rec {
    fetchNixpkgs = throw "Please use niv to pin nixpkgs instead.";
    # Equivalent of <nixpkgs> but pinned instead of system
    inherit (sources) nixpkgs;
    inherit pkgsDefault;
    getPkgs = let
      system' = system;
      config' = config;
      crossSystem' = crossSystem;
    in { args ? {}
       , extraOverlays ? nixpkgsOverlays
       , system ? system'
       , config ? config'
       , crossSystem ? crossSystem' }: import nixpkgs ({
            config = config;
            overlays = extraOverlays;
            inherit system crossSystem;
          } // args);
    getPkgsDefault = let
      system' = system;
      config' = config;
      crossSystem' = crossSystem;
    in { args ? {}
       , system ? system'
       , config ? config'
       , crossSystem ? crossSystem' }: import nixpkgs ({
            inherit system crossSystem config;
          } // args);
    pkgs = getPkgs {};
    getPackages = pkgs.callPackage ./get-packages.nix {};
    maybeEnv = import ./maybe-env.nix;

    commitIdFromGitRepo = upstreamedDeprecation "commitIdFromGitRepo" pkgs.lib.commitIdFromGitRepo;
    # A variant of lib.commitIdFromGitRepo which provides a default rev, instead of
    # throwing an exception in cases of error.
    # Example usage: commitIdFromGitRepoOrZero ./.git
    commitIdFromGitRepoOrZero = path:
      let
        zero = "0000000000000000000000000000000000000000";
        res = builtins.tryEval (pkgs.lib.commitIdFromGitRepo path);
      in
        if builtins.pathExists path
          then (if res.success then res.value else zero)
          else zero;

    # Development tools
    inherit (haskell-nix-extra-packages) stack-hpc-coveralls hpc-coveralls;
    hlint = upstreamedDeprecation "hlint" pkgsDefault.hlint;
    openapi-spec-validator = upstreamedDeprecation "openapi-spec-validator" pkgsDefault.python3Packages.openapi-spec-validator;
    inherit (import sources.cardano-repo-tool {inherit system;}) cardano-repo-tool;
    stack-cabal-sync-shell = pkgsDefault.callPackage ./pkgs/stack-cabal-sync-shell.nix { inherit cardano-repo-tool; };
    supervisord = pkgsDefault.callPackage ./supervisord {};
    fixStylishHaskell = pkgsDefault.callPackage ./tests/fix-stylish-haskell.nix {};

    # Check scripts
    check-hydra = builtins.trace "check-hydra is deprecated. Please use hydraEvalErrors" pkgsDefault.callPackage ./ci/check-hydra.nix {};
    checkStackProject = pkgsDefault.callPackage ./ci/check-stack-project.nix {};
    hydraEvalErrors = pkgsDefault.callPackage ./ci/hydra-eval-errors {};
    checkRepoTagsOnMasterBranches = pkgsDefault.callPackage ./ci/check-source-repo-tags-on-master.nix {};
    inherit (pkgsDefault.callPackage ./ci/cabal-project-regenerate {}) cabalProjectRegenerate checkCabalProject;
  };


  cardanoLib = commonLib.pkgsDefault.callPackage ./cardano-lib {};
  jormungandrLib = commonLib.pkgsDefault.callPackage ./jormungandr-lib { inherit rust-packages; };

  tests = {
    hlint = ./tests/hlint.nix;
    shellcheck = ./tests/shellcheck.nix;
    stylish-haskell = ./tests/stylish-haskell.nix;
  };

  overlays = {
    rust-packages = rust-packages.overlays;
    haskell-nix-extra = [(import ./overlays/haskell-nix-extra)];
    crypto = [(import ./overlays/crypto)];
    iohkNix = [(pkgs: super: {
      iohkNix = import ./. {
        inherit (pkgs) config system;
        pkgsDefault = pkgs;
      };
      iohk-nix.lib = import ./lib pkgs.lib;
    })];
    utils = [(import ./overlays/utils)];
  };

  rust-packages = rec {
    overlays = [
      (commonLib.pkgsDefault.callPackage ./overlays/rust/mozilla.nix {})
      (import ./overlays/rust)
    ];
    pkgs = import sources."nixpkgs-19.09" {
      inherit overlays config system crossSystem;
    };
  };

  # This attribute is here for iohk-nix/release.nix Hydra builds.
  # Projects should generally use the haskell-nix-extra overlay directly.
  haskell-nix-extra-packages = let
    baseOverlays = overlays;
    baseConfig = config;
    haskellNix = (import defaultSources."haskell.nix" {
      inherit system sourcesOverride;
    }).nixpkgsArgs;
  in rec {
    overlays = haskellNix.overlays ++ baseOverlays.haskell-nix-extra;
    config = haskellNix.config // baseConfig;
    pkgs = import defaultSources.nixpkgs {
      inherit overlays config system crossSystem;
    };
    inherit (pkgs) stackNixRegenerate haskellBuildUtils;
  } // pkgsDefault.lib.genAttrs
    [ "stack-hpc-coveralls" "hpc-coveralls" ]
    (pkg: throw "ERROR: iohk-nix `haskell-nix-extra-packages.${pkg}` has been removed.");

  shell = import ./shell.nix;

  self = {
    inherit
      overlays
      sources
      shell
      tests
      rust-packages
      haskell-nix-extra-packages
      cardanoLib
      jormungandrLib;

    inherit (pkgsDefault)
      niv;

    inherit (commonLib)
      # Package sets
      nixpkgs
      pkgs
      pkgsDefault

      # Library functions
      fetchNixpkgs
      getPkgs
      getPkgsDefault
      getPackages
      maybeEnv
      commitIdFromGitRepo
      commitIdFromGitRepoOrZero
      cabalProjectRegenerate
      supervisord

      # Packages
      stack-hpc-coveralls
      hpc-coveralls
      hlint
      stylish-haskell
      openapi-spec-validator
      cardano-repo-tool
      stack-cabal-sync-shell

      # Scripts
      check-hydra
      checkCabalProject
      hydraEvalErrors
      checkRepoTagsOnMasterBranches
      checkStackProject
      fixStylishHaskell;
    release-lib = ./lib/release-lib.nix;
  };
in self
