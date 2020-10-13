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
, defaultSources ? import ./nix/sources.nix
, pkgsDefault ? import defaultSources.nixpkgs { inherit config system crossSystem; }
}:

let
  upstreamedDeprecation = p: __trace "WARNING: commonLib.${p} is deprecated. Please use it from nixpkgs directly instead.";
  fetchTarballFromJson = jsonFile:
    let
      spec = builtins.fromJSON (builtins.readFile jsonFile);
    in builtins.fetchTarball {
      url = "${spec.url}/archive/${spec.rev}.tar.gz";
      inherit (spec) sha256;
    };
  deprecationWarning = parameter: builtins.trace ''
    WARNING: iohk-nix \"${parameter}\" parameter is deprecated.
    Please use niv (https://github.com/input-output-hk/niv/) and the \"sourcesOverride\" parameter instead.
  '';
  sources = defaultSources // sourcesOverride;

  inherit (import defaultSources.niv { pkgs = pkgsDefault; }) niv;

  commonLib = rec {
    fetchNixpkgs = throw "Please use niv to pin nixpkgs instead.";
    # equivalent of <nixpkgs> but pinned instead of system
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

    # Example usage: commitIdFromGitRepo ./.git
    commitIdFromGitRepo = pkgs.callPackage ./commit-id.nix {};
    # A variant of the above which provides a default rev, instead of
    # throwing an exception in cases of error.
    commitIdFromGitRepoOrZero = path:
      let
        zero = "0000000000000000000000000000000000000000";
        res = builtins.tryEval (commitIdFromGitRepo path);
      in
        if builtins.pathExists path
          then (if res.success then res.value else zero)
          else zero;

    # Development tools
    inherit (haskell-nix-extra-packages) stack-hpc-coveralls hpc-coveralls;
    hlint = upstreamedDeprecation "hlint" pkgsDefault.hlint;
    openapi-spec-validator = upstreamedDeprecation "openapi-spec-validator" pkgsDefault.python37Packages.openapi-spec-validator;
    inherit (import sources.cardano-repo-tool {inherit system;}) cardano-repo-tool;
    stack-cabal-sync-shell = pkgsDefault.callPackage ./pkgs/stack-cabal-sync-shell.nix { inherit cardano-repo-tool; };
    supervisord = pkgsDefault.callPackage ./supervisord {};

    # Check scripts
    check-hydra = __trace "check-hydra is deprecated. Please use hydraEvalErrors" pkgsDefault.callPackage ./ci/check-hydra.nix {};
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
    iohkNix = [(pkgs: super: rec {
      iohkNix = import ./. {
        inherit (pkgs) config system;
        pkgsDefault = pkgs;
      };
      inherit (iohkNix) niv;
    })];
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

  haskell-nix-extra-packages =
    let haskellNix = (import defaultSources."haskell.nix" { inherit system sourcesOverride; }).nixpkgsArgs;
    in with (import defaultSources.nixpkgs {
      inherit system crossSystem;
      config = haskellNix.config // config;
      overlays = haskellNix.overlays ++ overlays.haskell-nix-extra;
      });
    { inherit
        haskellBuildUtils
        stackNixRegenerate
        stack-hpc-coveralls
        hpc-coveralls
      ;
    };

  shell = import ./shell.nix;

  self = {
    inherit
      overlays
      sources
      niv
      shell
      tests
      rust-packages
      haskell-nix-extra-packages
      cardanoLib
      jormungandrLib;

    inherit (commonLib)
      # package sets
      nixpkgs
      pkgs
      pkgsDefault

      # library functions
      fetchNixpkgs
      getPkgs
      getPkgsDefault
      getPackages
      maybeEnv
      commitIdFromGitRepo
      commitIdFromGitRepoOrZero
      cabalProjectRegenerate
      supervisord

      # packages
      stack-hpc-coveralls
      hpc-coveralls
      hlint
      stylish-haskell
      openapi-spec-validator
      cardano-repo-tool
      stack-cabal-sync-shell

      # scripts
      check-hydra
      checkCabalProject
      hydraEvalErrors
      checkRepoTagsOnMasterBranches
      checkStackProject;
    release-lib = ./lib/release-lib.nix;
  };
in self
