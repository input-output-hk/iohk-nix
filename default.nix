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
# Override haskell-nix.json to a file in your repo
, haskellNixJsonOverride ? ""
, defaultSources ? import ./nix/sources.nix
, pkgsDefault ? import defaultSources.nixpkgs { inherit config system crossSystem; }
}:

let
  nixToolsDeprecation = __trace "WARNING: nix-tools integration is deprecated. Please upgrade haskell.nix and use overlays instead";
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
  hasNoPathOverride = host: !(builtins.tryEval (builtins.findFile builtins.nixPath host)).success;
  sources = with pkgsDefault.lib; defaultSources // sourcesOverride //
    (optionalAttrs (hasNoPathOverride "custom_nixpkgs")
      (optionalAttrs (application != "") {
        nixpkgs = deprecationWarning "application"
          defaultSources."nixpkgs-${application}";
      }) //
      (optionalAttrs (nixpkgsJsonOverride != "") {
        nixpkgs = deprecationWarning "nixpkgsJsonOverride"
          fetchTarballFromJson nixpkgsJsonOverride;
      }) //
      (optionalAttrs (nixpkgsOverride != "") {
        nixpkgs = deprecationWarning "nixpkgsOverride"
          nixpkgsOverride;
      })
    ) //
    (optionalAttrs (hasNoPathOverride "haskell" && haskellNixJsonOverride != "") {
      haskell = deprecationWarning "haskellNixJsonOverride"
        fetchTarballFromJson haskellNixJsonOverride;
    });

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
      haskellNixSrc = import sources."haskell.nix";
    in { args ? {}
       , extraOverlays ? nixpkgsOverlays
       , system ? system'
       , config ? config'
       , crossSystem ? crossSystem' }: import nixpkgs ({
            config = haskellNixSrc.config // config;
            overlays = haskellNixSrc.overlays ++ extraOverlays;
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
    cleanSourceHaskell = pkgs.callPackage ./clean-source-haskell.nix {};
    haskellPackages = import ./haskell-packages.nix;

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
    haskellBuildUtils = import ./utils/default.nix {
      pkgs = import defaultSources.nixpkgs { inherit system; };
    };
    cache-s3 = pkgsDefault.callPackage ./pkgs/cache-s3.nix {};
    stack-hpc-coveralls = pkgsDefault.haskellPackages.callPackage ./pkgs/stack-hpc-coveralls.nix {};
    hlint = upstreamedDeprecation "hlint" pkgsDefault.hlint;
    openapi-spec-validator = upstreamedDeprecation "openapi-spec-validator" pkgsDefault.python37Packages.openapi-spec-validator;
    inherit (import sources.cardano-repo-tool {inherit system;}) cardano-repo-tool;
    stack-cabal-sync-shell = pkgsDefault.callPackage ./pkgs/stack-cabal-sync-shell.nix { inherit cardano-repo-tool; };
    stylish-haskell = pkgsDefault.callPackage ./pkgs/stylish-haskell.nix {
      haskell = nix-tools.default-haskell-nix;
    };

    # Check scripts
    check-hydra = __trace "check-hydra is deprecated. Please use hydraEvalErrors" pkgsDefault.callPackage ./ci/check-hydra.nix {};
    check-nix-tools = pkgsDefault.callPackage ./ci/check-nix-tools.nix {};
    hydraEvalErrors = pkgsDefault.callPackage ./ci/hydra-eval-errors {};
    inherit (pkgsDefault.callPackage ./ci/cabal-project-regenerate {}) cabalProjectRegenerate checkCabalProject;
  };


  cardanoLib = commonLib.pkgsDefault.callPackage ./cardano-lib {};
  jormungandrLib = commonLib.pkgsDefault.callPackage ./jormungandr-lib { inherit rust-packages; };

  nix-tools = let pkgs = import defaultSources.nixpkgs (import defaultSources."haskell.nix");
    in rec {
    default-haskell-nix = pkgs.haskell-nix;
    # Programs for generating nix haskell package sets from cabal and
    # stack.yaml files.
    package = nixToolsDeprecation default-haskell-nix.nix-tools;
    # A different haskell infrastructure
    haskell = _: nixToolsDeprecation (commonLib.getPkg {}).haskell-nix;
    # Script to invoke nix-tools stack-to-nix on a repo.
    regenerateStackPackages = pkgs.callPackage ./nix-tools-regenerate.nix {
      nix-tools = default-haskell-nix.nix-tools;
    };
    # default and release templates that abstract
    # over the details for CI.
    default-nix = nixToolsDeprecation import ./nix-tools-default.nix (commonLib // { inherit nix-tools; });
    release-nix = nixToolsDeprecation import ./nix-tools-release.nix (commonLib // { inherit nix-tools; });

    # default iohk module and extras to be used in the pkgs.nix file of the
    # project.  The module will provide the necessary default overrides for
    # packages (patches) to work properly in cross compiled settings.
    iohk-module = nixToolsDeprecation import ./nix-tools-iohk-module.nix commonLib;
    # The extras provide the necessary extra packages that might be missing
    # from generated plans (mostly from stackage snapshot) as well as patches
    # to align the packages downloaded from hackage with what GHC ships as those
    # packages.  That a package of a given version on hackage is identical to
    # the package that ghc ships with the same version is not a given!
    iohk-extras = nixToolsDeprecation import ./nix-tools-iohk-extras.nix commonLib;
  };

  stack2nix = rec {
    regenerateStackPackages = {hackageSnapshot}: __trace "NOTICE: stack2nix is deprecated. Please switch to haskell.nix" commonLib.pkgsDefault.callPackage ./stack2nix-regenerate.nix {
      inherit hackageSnapshot;
    };
  };

  tests = {
    hlint = ./tests/hlint.nix;
    shellcheck = ./tests/shellcheck.nix;
    stylishHaskell = ./tests/stylish-haskell.nix;
  };

  overlays = {
    haskell-nix-extra = [(import ./overlays/haskell-nix-extra.nix)];
    rust-packages = rust-packages.overlays;
    iohkNix = [(pkgs: super: rec {
      iohkNix = import ./. {
        inherit (pkgs) config system;
        pkgsDefault = pkgs;
      };
      inherit (iohkNix) niv;
      haskellPackages = super.haskellPackages // {
        inherit (iohkNix) niv;
      };
    })];
  };

  rust-packages = rec {
    overlays = [
      (commonLib.pkgsDefault.callPackage ./overlays/rust/mozilla.nix {})
      (import ./overlays/rust)
    ];
    pkgs = import sources.nixpkgs {
      inherit overlays config system crossSystem;
    };
  };

  shell = import ./shell.nix;

  self = {
    inherit
      overlays
      sources
      niv
      shell
      tests
      nix-tools
      stack2nix
      rust-packages
      cardanoLib
      jormungandrLib;

    inherit (commonLib)
      # package sets
      nixpkgs
      pkgs
      pkgsDefault
      haskellPackages

      # library functions
      fetchNixpkgs
      getPkgs
      getPkgsDefault
      getPackages
      maybeEnv
      cleanSourceHaskell
      commitIdFromGitRepo
      commitIdFromGitRepoOrZero
      cabalProjectRegenerate

      # packages
      cache-s3
      stack-hpc-coveralls
      hlint
      stylish-haskell
      openapi-spec-validator
      cardano-repo-tool
      stack-cabal-sync-shell
      haskellBuildUtils

      # scripts
      check-hydra
      checkCabalProject
      check-nix-tools
      hydraEvalErrors;
    release-lib = ./lib/release-lib.nix;
  };
in self
