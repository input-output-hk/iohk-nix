# ChangeLog

Please read these notes when updating your project's `iohk-nix`
version. There may have been changes which could break your build.

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
