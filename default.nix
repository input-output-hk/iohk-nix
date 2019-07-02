{ globalConfig ? import ./config.nix
, config ? {}
, system ? builtins.currentSystem
, crossSystem ? null
, sourcesOverride ? {}
# Set application for getting a specific application nixkgs-src.json
, application ? ""
, nixpkgsOverride ? ""
# Modify nixpkgs with overlays
, nixpkgsOverlays ? []
}:

let
  defaultSources = import ./nix/sources.nix;
  sources = defaultSources // sourcesOverride;

  jemallocOverlay = import ./overlays/jemalloc.nix;

  commonLib = rec {
    # equivalent of <nixpkgs> but pinned instead of system
    nixpkgs = builtins.getAttr
      (if application != "" then "nixpkgs-${application}" else "nixpkgs")
      sources;
    pkgsDefault = import (defaultSources.nixpkgs) {};
    getPkgs = let
      system' = system;
      globalConfig' = globalConfig;
      config' = config;
      crossSystem' = crossSystem;
    in { args ? {}
       , extraOverlays ? nixpkgsOverlays
       , system ? system'
       , globalConfig ? globalConfig'
       , config ? config'
       , crossSystem ? crossSystem' }: import nixpkgs ({
          overlays = [ jemallocOverlay ] ++ extraOverlays;
          config = globalConfig // config;
          inherit system crossSystem;
          } // args);
    pkgs = getPkgs {};
    getPackages = pkgs.callPackage ./get-packages.nix {};
    maybeEnv = import ./maybe-env.nix;
    cleanSourceHaskell = pkgs.callPackage ./clean-source-haskell.nix {};
    haskellPackages = import ./haskell-packages.nix;
    commitIdFromGitRepo = pkgs.callPackage ./commit-id.nix {};

    # Development tools
    haskellBuildUtils = import ./utils/default.nix { pkgs = pkgsDefault; };
    cache-s3 = pkgsDefault.callPackage ./pkgs/cache-s3.nix {};
    stack-hpc-coveralls = pkgsDefault.haskellPackages.callPackage ./pkgs/stack-hpc-coveralls.nix {};
    hlint = pkgsDefault.haskellPackages.callPackage ./pkgs/hlint.nix {};
    openapi-spec-validator = pkgsDefault.python3Packages.callPackage ./pkgs/openapi-spec-validator.nix {
      # Upstream PR: https://github.com/NixOS/nixpkgs/pull/65244
      # It requires PyYAML >= 5.1.
      pyyaml = pkgsDefault.python3Packages.callPackage ./pkgs/pyyaml51.nix {};
    };
    cardano-repo-tool = pkgsDefault.callPackage ./pkgs/cardano-repo-tool.nix {
      haskell = nix-tools.haskell { pkgs = pkgsDefault; };
    };
    stylish-haskell = pkgsDefault.callPackage ./pkgs/stylish-haskell.nix {
      haskell = nix-tools.haskell { pkgs = pkgsDefault; };
    };

    # Check scripts
    check-hydra = pkgsDefault.callPackage ./ci/check-hydra.nix {};
    check-nix-tools = pkgsDefault.callPackage ./ci/check-nix-tools.nix {};
  };

  cardanoLib = commonLib.pkgsDefault.callPackage ./cardano-lib {};
  jormungandrLib = commonLib.pkgsDefault.callPackage ./jormungandr-lib {};

  nix-tools = rec {
    # Programs for generating nix haskell package sets from cabal and
    # stack.yaml files.
    package = (haskell { pkgs = commonLib.pkgsDefault; }).nix-tools;
    # A different haskell infrastructure
    haskell = { pkgs }: import sources.haskell { inherit pkgs; };
    # Script to invoke nix-tools stack-to-nix on a repo.
    regeneratePackages = commonLib.pkgsDefault.callPackage ./nix-tools-regenerate.nix {
      nix-tools = package;
    };
    # default and release templates that abstract
    # over the details for CI.
    default-nix = import ./nix-tools-default.nix (commonLib // { inherit nix-tools; });
    release-nix = import ./nix-tools-release.nix (commonLib // { inherit nix-tools; });

    # default iohk module and extras to be used in the pkgs.nix file of the
    # project.  The module will provide the necessary default overrides for
    # packages (patches) to work properly in cross compiled settings.
    iohk-module = import ./nix-tools-iohk-module.nix commonLib;
    # The extras provide the necessary extra packages that might be missing
    # from generated plans (mostly from stackage snapshot) as well as patches
    # to align the packages downloaded from hackage with what GHC ships as those
    # packages.  That a package of a given version on hackage is identical to
    # the package that ghc ships with the same version is not a given!
    iohk-extras = import ./nix-tools-iohk-extras.nix commonLib;
  };

  stack2nix = rec {
    regeneratePackages = {hackageSnapshot}: commonLib.pkgsDefault.callPackage ./stack2nix-regenerate.nix {
      inherit hackageSnapshot;
    };
  };

  tests = {
    hlint = ./tests/hlint.nix;
    shellcheck = ./tests/shellcheck.nix;
    stylishHaskell = ./tests/stylish-haskell.nix;
  };

  rust-packages = rec {
    overlays = [
      (commonLib.pkgsDefault.callPackage ./overlays/rust/mozilla.nix {})
      (import ./overlays/rust)
    ];
    pkgs = import sources.nixpkgs-unstable {
      inherit overlays;
      config = globalConfig // config;
      inherit system crossSystem;
    };
  };

in {
  inherit tests nix-tools stack2nix jemallocOverlay rust-packages cardanoLib jormungandrLib;
  inherit (commonLib) pkgs haskellPackages fetchNixpkgs maybeEnv cleanSourceHaskell getPkgs nixpkgs commitIdFromGitRepo getPackages cache-s3 stack-hpc-coveralls hlint stylish-haskell openapi-spec-validator cardano-repo-tool check-hydra check-nix-tools haskellBuildUtils;
  release-lib = ./lib/release-lib.nix;
  inherit (import sources.niv {}) niv;
}
