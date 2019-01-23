# This file contains patches across haskell packages that
# amend the packages to be cross-compilable to windows,
# and potentially later other targets.
#
# The module is supposed to be used as part of the pkgSet's
# `modules` setion:
# Example:
#    modules = [
#      haskell.ghcHackagePatches.${(stack-pkgs.overlay hackage).compiler.nix-name}
#      iohk-module
#    ];
#
commonLib:
{
  # clock hasn't had a release since 2016(!) that is for three(3) years
  # now.
  packages.clock.patches = [ ({ version, revision }: if version == "0.7.2" then ./patches/clock-0.7.2.patch else null) ];
  # nix calles this package crypto
  packages.cryptonite-openssl.patches = [ ({ version, revision }: if version == "0.7" then ./patches/cryptonite-openssl-0.7.patch else null) ];
}
