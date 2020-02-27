{ pkgs
}:

with pkgs;

let
  cabal2nixAllowCache = self: super: {
    haskellSrc2nix = args: (super.haskellSrc2nix args).overrideAttrs (old: {
      allowSubstitutes = true;
    });
  };
in
rec {
  # The utils derivation.
  package = (buildPackages.haskellPackages.extend cabal2nixAllowCache).callCabal2nix "iohk-nix-utils" ./. {};

  # A function for building a script that can run in CI.
  # The given script path, which is Haskell file will be able to
  # import the CommonBuild module.
  stackRebuild =
   { script           # path to .hs file for building
   , buildTools ? []  # extra programs to add to PATH
   , libs ? (ps: [])  # selector function for extra haskell libraries
   , shell ? null     # shell derivation to keep gc-rooted during build
   }:
   let
      buildTools' = [
        gnused coreutils git nix gnumake gnutar gzip lz4
        stack pkgs.stack-hpc-coveralls
        haskellPackages.weeder
      ] ++ buildTools;
      libs' = ps: libs ps ++ [ package ];
      ghc' = haskellPackages.ghcWithPackages libs';

   in runCommand "stack-rebuild" {
        buildInputs = [ ghc' makeWrapper ];
    } ''
      mkdir -p $out/bin
      ghc -Wall -threaded -o $out/bin/rebuild ${script}
      wrapProgram $out/bin/rebuild \
        --set PATH "${lib.makeBinPath buildTools'}" \
        --set NO_GC_STACK_SHELL "${toString shell}"
    '';
}

