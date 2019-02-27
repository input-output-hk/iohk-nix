{ globalConfig ? import ./config.nix
, config ? {}
, system ? builtins.currentSystem
, crossSystem ? null
# Set application for getting a specific application nixkgs-src.json
, application ? ""
# Override nixpkgs-src.json to a file in your repo
, nixpkgsJsonOverride ? ""
}:

let
  # Default nixpkgs-src.json to use
  nixpkgsJsonDefault = ./pins/default-nixpkgs-src.json;
  nixpkgsJson = if (nixpkgsJsonOverride != "") then nixpkgsJsonOverride else
    (if (application != "") then (getNixpkgsJson application) else nixpkgsJsonDefault);

  getNixpkgsJson = application: ./pins + "/${application}-nixpkgs-src.json";
  jemallocOverlay = import ./overlays/jemalloc.nix;

  commonLib = rec {
    fetchNixpkgs = import ./fetch-tarball-with-override.nix "custom_nixpkgs";
    # equivalent of <nixpkgs> but pinned instead of system
    nixpkgs = fetchNixpkgs nixpkgsJson;
    pkgsDefault = import (fetchNixpkgs nixpkgsJsonDefault) {};
    getPkgs = let
      system' = system;
      globalConfig' = globalConfig;
      config' = config;
      crossSystem' = crossSystem;
    in { args ? {}
       , extraOverlays ? []
       , system ? system'
       , globalConfig ? globalConfig'
       , config ? config'
       , crossSystem ? crossSystem' }: import (fetchNixpkgs nixpkgsJson) ({
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
  };

  nix-tools = rec {
    # Programs for generating nix haskell package sets from cabal and
    # stack.yaml files.
    package = (haskell { pkgs = commonLib.pkgsDefault; }).nix-tools;
    # A different haskell infrastructure
    haskell = import ./haskell.nix;
    # Script to invoke nix-tools stack-to-nix on a repo.
    regeneratePackages = commonLib.pkgsDefault.callPackage ./nix-tools-regenerate.nix {
      nix-tools = package;
    };
    # default and release templates that abstract
    # over the details for CI.
    default-nix = import ./nix-tools-default.nix (commonLib // { inherit nix-tools; });
    release-nix = import ./nix-tools-release.nix (commonLib // { inherit nix-tools; });

    # default iohk module and overlay to be used in the pkgs.nix file of the
    # project.  The module will provide the necessary default overrides for
    # packages (patches) to work properly in cross compiled settings.
    iohk-module = import ./nix-tools-iohk-module.nix commonLib;
    # The overlay provides the necessary extra packages that might be missing
    # from generated plans (mostly from stackage snapshot) as well as patches
    # to align the packages downloaded from hackage with what GHC ships as those
    # packages.  That a package of a given version on hackage is identical to
    # the package that ghc ships with the same version is not a given!
    iohk-overlay = import ./nix-tools-iohk-overlay.nix commonLib;
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

in {
  inherit tests nix-tools stack2nix jemallocOverlay;
  inherit (commonLib) pkgs haskellPackages fetchNixpkgs maybeEnv cleanSourceHaskell getPkgs nixpkgs commitIdFromGitRepo getPackages;
}
