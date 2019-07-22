# ChangeLog

Please read these notes when updating your project's `iohk-nix`
version. There may have been changes which could break your build.

## 2019-07-22
   * The `check-nix-tools` CI script (run by `nix/regenerate.sh`) has been updated in
     [PR #131](https://github.com/input-output-hk/iohk-nix/pull/131).
     It should work in much the same way as before, but can push fixes (changes in generated nix code)
     to PR branches if credentials for the repo have been installed on
     the Buildkite agents.
   
## 2019-04-09

   * Started changelog
