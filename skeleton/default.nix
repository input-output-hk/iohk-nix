############################################################################
# iohk-skeleton Nix build
#
# fixme: document top-level attributes and how to build them
#
############################################################################

{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
# Import IOHK common nix lib
, commonLib ? import ./lib.nix
# Use nixpkgs pin from commonLib
, pkgs ? commonLib.pkgs
}:

let
  haskell = pkgs.callPackage commonLib.nix-tools.haskell {};
  src = commonLib.cleanSourceHaskell ./.;
  util = pkgs.callPackage ./nix/util.nix {};

  # Example of using a package from iohk-nix
  # TODO: Declare packages required by the build.
  inherit (commonLib.rust-packages.pkgs) jormungandr;

  # Import the Haskell package set.
  haskellPackages = import ./nix/pkgs.nix {
    inherit pkgs haskell src;
    # Pass in any extra programs necessary for the build as function arguments.
    # TODO: Declare packages required by the build.
    # jormungandr and cowsay are just examples and should be removed for your
    # project, unless needed.
    inherit jormungandr;
    inherit (pkgs) cowsay;
    # Provide cross-compiling secret sauce
    inherit (commonLib.nix-tools) iohk-extras iohk-module;
  };

in {
  inherit pkgs commonLib src haskellPackages;
  inherit (haskellPackages.iohk-skeleton.identifier) version;

  # Grab the executable component of our package.
  inherit (haskellPackages.iohk-skeleton.components.exes)
    iohk-skeleton;

  tests = util.collectComponents "tests" util.isIohkSkeleton haskellPackages;
  benchmarks = util.collectComponents "benchmarks" util.isIohkSkeleton haskellPackages;

  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  shell = haskellPackages.shellFor {
    name = "iohk-skeleton-shell";
    # TODO: List all local packages in the project.
    packages = ps: with ps; [
      iohk-skeleton
    ];
    # These programs will be available inside the nix-shell.
    buildInputs =
      with pkgs.haskellPackages; [ hlint stylish-haskell weeder ghcid lentil ]
      # TODO: Add your own packages to the shell.
      ++ [ jormungandr ];
  };

  # Example of a linting script used by Buildkite.
  checks.lint-fuzz = pkgs.callPackage ./nix/check-lint-fuzz.nix {};

  # Attrset of PDF builds of LaTeX documentation.
  docs = pkgs.callPackage ./docs/default.nix {};
}
