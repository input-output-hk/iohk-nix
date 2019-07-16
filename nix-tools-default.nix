commonLib:      # the iohk-nix commonLib, that provides access to the pinned packages
nix-tools-pkgs: # a function (or the path to the local pkgs.nix file for nix-tools) that imports all
                # haskell specific data.

{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, pkgs ? commonLib.getPkgs { inherit system crossSystem config; }
}:
with builtins; with pkgs.lib;
let  nix-tools = (if builtins.isFunction nix-tools-pkgs
                    then nix-tools-pkgs
                    else import nix-tools-pkgs) {
  haskell = commonLib.nix-tools.haskell {
    # We need to pass `pkgs` here, otherwise we loose all
    # config customizations that are essential.
    inherit pkgs;
  };
  inherit pkgs;
  # the iohk-module contains cross compilation specific patches
  inherit (commonLib.nix-tools) iohk-module iohk-extras;
  };
  # Allow hsPkgs to be returned separately (useful when using
  # callCabalPlanToNix and callStackToNix on Hydra)
  hsPkgs = nix-tools.hsPkgs or nix-tools;
in {
    _lib = commonLib;

    # Unlike `nix-tools._raw`, accessing `nix-tools-raw` does not force
    # the components to be enumerated.
    nix-tools-raw = nix-tools;

    # This will allow us to build
    # nix-tools.libs.cardano-chain to obtain all libs in a single derivation
    # nix-tools.exes.cardano-chain for all executables, same for tests and benchmarks.
    #
    # The alternative syntax is: nix-tools._raw.cardano-chain.components.$comp
    # if you want to build only a single component.
    nix-tools = { _raw = nix-tools; }
      # some shorthands
      // {
        inherit (hsPkgs) shellFor;
      }
      // { libs = mapAttrs (k: v: if   v ? components && v.components ? "library"
                                  then v.components.library
                                  else null) hsPkgs; }
      // { exes = mapAttrs (k: v: if   (v ? components && length (attrValues v.components.exes) > 0)
                                  then (if pkgs.stdenv.targetPlatform.isWindows then pkgs.copyJoin else pkgs.symlinkJoin)
                                       { name = "${k}-exes"; paths = attrValues v.components.exes; }
                                  else null) hsPkgs; }
      // { cexes = mapAttrs (k: v: if v ? components && length (attrValues v.components.exes) > 0
                                  then v.components.exes
                                  else null) hsPkgs; }
      // { tests = mapAttrs (k: v: if v ? components && length (attrValues v.components.tests) > 0
                                   then v.components.tests
                                   else null) nix-tools; }
      // { benchmarks = mapAttrs (k: v: if v ? components && length (attrValues v.components.benchmarks) > 0
                                   then v.components.benchmarks
                                   else null) hsPkgs; }
      ;
  }
