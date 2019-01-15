let
  # The Haskell generic builder falls back to checking 'isExecutable' if 'isLibrary' 
  # is not provided. Tools like cabal2nix therefore often don't provide 'isLibrary', so
  # we need to mimic that logic here.
  isExecutable = args: args.isExecutable or false;
  isLibrary = args: args.isLibrary or (!(isExecutable args));
  isBenchmark = args: !((isExecutable args) || (isLibrary args));

in { pkgs, filter }:

with pkgs.lib;

self: super: {
  mkDerivation = args: super.mkDerivation (args // optionalAttrs (filter args.pname) {
    # Enables building but not running of benchmarks for all
    doBenchmark = true;
    configureFlags = (args.configureFlags or []) ++ ["--enable-benchmarks"];
  } // optionalAttrs (isBenchmark args) {
    # Provide a dummy installPhase for benchmark packages.
    installPhase = "mkdir -p $out";
  });
}
