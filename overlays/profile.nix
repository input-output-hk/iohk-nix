{ pkgs, filter }:

with pkgs.lib;

self: super: {
  mkDerivation = args: super.mkDerivation (args // optionalAttrs (filter args.pname) {
    enableLibraryProfiling = true;
    enableExecutableProfiling = true;
  });
}
