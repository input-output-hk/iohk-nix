self: super: {
  # jemalloc has a bug that causes link failures
  # https://github.com/jemalloc/jemalloc/issues/937
  # Using jemalloc 510 with the --disable-initial-exec-tls flag seems to
  # fix it.
  jemalloc = self.callPackage ../jemalloc/jemalloc510.nix {};
}
