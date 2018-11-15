{ config ? {}
, system ? builtins.currentSystem
# Set application for getting a specific application nixkgs-src.json
, application ? ""
# Override nixpkgs-src.json to a file in your repo
, nixpkgsJsonOverride ? ""
}:

let
  # Default nixpkgs-src.json to use
  nixpkgsJsonDefault = ./nixpkgs-src.json;
  nixpkgsJson = if (nixpkgsJsonOverride != "") then nixpkgsJsonOverride else
    (if (application != "") then (getNixpkgsJson application) else nixpkgsJsonDefault);

  getNixpkgsJson = application: ./. + "/${application}/nixpkgs-src.json";
  jemallocOverlay = import ./overlays/jemalloc.nix;

  commonLib = rec {
    fetchNixpkgs = import ./fetch-nixpkgs.nix;
    getPackages = import ./get-packages.nix;
    # equivalent of <nixpkgs> but pinned instead of system
    nixpkgs = fetchNixpkgs nixpkgsJson;
    getPkgs = { args ? {}, extraOverlays ? [] }: import (fetchNixpkgs nixpkgsJson) ({
      overlays = [ jemallocOverlay ] ++ extraOverlays;
      inherit config system;
    } // args);
    pkgs = getPkgs {};
    maybeEnv = import ./maybe-env.nix;
    cleanSourceHaskell = pkgs.callPackage ./clean-source-haskell.nix {};
    haskellPackages = import ./haskell-packages.nix;
  };

  # Overlay which adds more functions to pkgs.lib
  extraLib = self: super: {
    lib = super.lib.extend (lib:_: {
      inherit (commonLib) cleanSourceHaskell;
      getPackages = commonLib.pkgs.callPackage ./get-packages.nix {};
      commitIdFromGitRepo = commonLib.pkgs.callPackage ./commit-id.nix {};
    });
  };


  tests = {
    hlint = ./tests/hlint.nix;
    shellcheck = ./tests/shellcheck.nix;
    stylishHaskell = ./tests/stylish-haskell.nix;
  };

  # Specific package tooling. If you have to add something to iohk-nix
  # that only applies to your project, do it here:

  cardano-sl = rec {
    pkgs = commonLib.getPkgs {
      extraOverlays = [
        extraLib
      ];
    };
    haskellPackages = args: commonLib.pkgs.callPackage ./haskell-packages.nix (args // { });
  };

  daedalus = {
    inherit (commonLib) pkgs;
  };

  # End specific packaging tooling

in {
  inherit tests daedalus cardano-sl application nixpkgsJson;
  inherit (commonLib) pkgs jemallocOverlay haskellPackages fetchNixpkgs maybeEnv cleanSourceHaskell getPkgs nixpkgs;
}
