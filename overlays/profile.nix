{ pkgs, filter, enableProfiling }:

with pkgs.lib;

# The nixpkgs defaults are to enable library profiling and disable executable profiling.
# We would like to enable both when profiling is enabled, and disable both when it is disabled,
# so we need this overlay unconditionally to set them both to the same, we can't leave
# it to the defaults.
self: super: {
  # Try and prevent rebuilds by leaving the original attribute in place: in particular, if it doesn't have the
  # attribute set and we're not enabling profiling, don't add the attribute set to false, as this will be a rebuild
  mkDerivation = args: super.mkDerivation (args // (optionalAttrs (args ? enableLibraryProfiling || enableProfiling) {
    # For non-filtered packages:
    # - Ignore executable profiling
    # - Control library profiling, but if we were going to already build with profiling then
    #   do that - it costs us nothing and reduces rebuilds.
    enableLibraryProfiling = (args ? enableLibraryProfiling && args.enableLibraryProfiling) || enableProfiling;
  }) //
  (optionalAttrs (filter args.pname) {
    # For filtered packages:
    # - Control executable profiling
    # - Control library profiling
    enableExecutableProfiling = enableProfiling;
    enableLibraryProfiling = enableProfiling;
  }));
}
