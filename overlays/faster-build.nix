# Disabling optimization for packages will
# return a build ~20% faster (measured in DEVOPS-1032).

{ pkgs, filter }:

with import ../../lib.nix;

self: super: {
  mkDerivation = args: super.mkDerivation (args // optionalAttrs (filter args.pname) {
    configureFlags = (args.configureFlags or []) ++ [ "--ghc-options=-O0" ];
  });
}
