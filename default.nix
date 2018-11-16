{ config ? {}
, system ? builtins.currentSystem
# Set application for getting a specific application nixkgs-src.json
, application ? ""
# Override nixpkgs-src.json to a file in your repo
, nixpkgsJsonOverride ? ""
}:

let
  # Default nixpkgs-src.json to use
  nixpkgsJsonDefault = ./nixpkgs-pins/default-nixpkgs-src.json;
  nixpkgsJson = if (nixpkgsJsonOverride != "") then nixpkgsJsonOverride else
    (if (application != "") then (getNixpkgsJson application) else nixpkgsJsonDefault);

  getNixpkgsJson = application: ./nixpkgs-pins + "/${application}-nixpkgs-src.json";
  jemallocOverlay = import ./overlays/jemalloc.nix;

  commonLib = rec {
    fetchNixpkgs = import ./fetch-nixpkgs.nix;
    # equivalent of <nixpkgs> but pinned instead of system
    nixpkgs = fetchNixpkgs nixpkgsJson;
    getPkgs = { args ? {}, extraOverlays ? [] }: import (fetchNixpkgs nixpkgsJson) ({
      overlays = [ jemallocOverlay ] ++ extraOverlays;
      inherit config system;
    } // args);
    pkgs = getPkgs {};
    getPackages = pkgs.callPackage ./get-packages.nix {};
    maybeEnv = import ./maybe-env.nix;
    cleanSourceHaskell = pkgs.callPackage ./clean-source-haskell.nix {};
    haskellPackages = import ./haskell-packages.nix;
    commitIdFromGitRepo = pkgs.callPackage ./commit-id.nix {};
  };

  tests = {
    hlint = ./tests/hlint.nix;
    shellcheck = ./tests/shellcheck.nix;
    stylishHaskell = ./tests/stylish-haskell.nix;
  };

in {
  inherit tests;
  inherit (commonLib) pkgs jemallocOverlay haskellPackages fetchNixpkgs maybeEnv cleanSourceHaskell getPkgs nixpkgs commitIdFromGitRepo getPackages;
}
