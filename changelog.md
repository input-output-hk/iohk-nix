# ChangeLog

Please read these notes when updating your project's `iohk-nix`
version. There may have been changes which could break your build.

## 2021-02-21
   * Reduce build closure size and the amount of code in iohk-nix.
   * Removed `haskell-nix-extra.stack-hpc-coveralls` - use
     ```
     haskell-nix.tool "ghc865" "stack-hpc-coveralls" "1.2.0"
     ```
     instead.
   * Removed `haskell-nix-extra.hpc-coveralls` - use
     ```
     haskell-nix.tool "ghc865" "hpc-coveralls" "0.0.4.0"
     ```
     instead.
   * Deprecated the `haskell-nix.haskellLib.extra.collectChecks` function
     in favour of `haskell-nix.haskellLib.collectChecks'`.
   * Removed `haskell-nix-extra.haskellBuildUtils.stackRebuild`.
   * Renamed `haskell-nix-extra.haskellBuildUtils.package`
     to  `haskell-nix-extra.haskellBuildUtils`.
   * `haskellBuildUtils` changed build system from `callCabal2nix`
     to Haskell.nix. It requires another overlay
     to provide `pkg.haskell-nix`.
   * When using `haskellBuildUtils`, also add `haskellBuildUtils.roots`
     to your `release.nix`, so that eval-time dependencies are cached.

## 2021-01-04
  * Switch default nixpkgs to nixos-unstable

## 2020-11-11
   * Switch default nixpkgs to 20.09
   * `commonLib.commitIdFromGitRepo` is deprecated in favour of nixpkgs `lib.commitIdFromGitRepo`.

## 2020-07-14
   * Bump Haskell.nix to latest. There are [multiple API changes](https://github.com/input-output-hk/haskell.nix/blob/master/changelog.md).
   * Fix `stackNixRegenerate` script for latest Haskell.nix.

## 2020-05-27
   * Switch default nixpkgs to 20.03
   * Remove skeleton (moved to https://github.com/input-output-hk/cardano-skeleton/)

## 2020-02-19
   * remove support for haskell.nix (use overlays instead)
   * removes numerous deprecations related to removal of haskell.nix support

## 2020-02-06
   * migrate skeleton to cabalProject and haskell.nix as overlay.

## 2020-01-14
   * Add a timeout parameter to the `Build.doBuild`, in `iohk-nix-utils`.

## 2019-10-27
   * Changes `mkRequired` of `release-lib` to return an attribute set
     containing `required` and `build-version`

## 2019-10-08

   * Switched to niv for source management instead of json files


## 2019-07-25

   * Added a [skeleton project](./skeleton/README.md) which provides a
     reference on how to set up iohk-nix CI for projects.

## 2019-07-23

   * new `disabled-jobs` parameter to `release-nix`: all jobs with a path
     that starts with one of the values in the `disabled-jobs` list are ignored
     (and no longer built by hydra).

## 2019-07-22

   * The `check-nix-tools` CI script (run by `nix/regenerate.sh`) has been updated in
     [PR #131](https://github.com/input-output-hk/iohk-nix/pull/131).
     It should work in much the same way as before, but can push fixes (changes in generated nix code)
     to PR branches if credentials for the repo have been installed on
     the Buildkite agents.

   * `release-nix` now provides jobs that respectively aggregates all libs, exes, tests and benchmarks of the project for each supported system, eg.:
     - nix-tools.packages-tests.x86_64-linux
     - nix-tools.packages-libs.x86_64-darwin
     - nix-tools.x86_64-pc-mingw32-packages-exes.x86_64-linux

## 2019-04-09

   * Started changelog
