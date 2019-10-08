# IOHK Common Nix Code

[![Build status](https://badge.buildkite.com/e5b12d0fd507084fbdb1849da2de467f1de66b3e5c6d954554.svg)](https://buildkite.com/input-output-hk/iohk-nix)

This repo contains build code and tools shared between IOHK projects.

1. Pinned versions of [input-output-hk/nixpkgs](https://github.com/input-output-hk/nixpkgs).
2. [Haskell.nix](https://github.com/input-output-hk/haskell.nix) and
   `nix-tools`, plus package overlays and patches for cross-compiling to
   Windows.
3. Scripts for regenerating code with `nix-tools`.
4. Util functions such as source filtering.
5. Nix builds of development tools such as HLint, ShellCheck, Stylish Haskell, SHC, cache-s3.
6. Nix packages and overlay for the [rust-cardano](https://github.com/input-output-hk/rust-cardano)
   projects.
7. A [skeleton project](./skeleton) demonstrating how to use iohk-nix
   and set up CI.


## How to use in your project

See [Starting a new project](./docs/start.md).


## How to update the `iohk-nix` revision.

niv update iohk-nix

Some things may have changed which could break your build, so refer to
the [ChangeLog](./changelog.md).


## How to use Haskell.nix and `stack-to-nix`

Look in the "User Guide" section of the [Haskell.nix
Documentation](https://input-output-hk.github.io/haskell.nix/).

## After updating your Stackage LTS

If you have updated your project's [Stackage LTS](https://www.stackage.org/lts)
version, then you may need to update the `iohk-nix` revision in your
project. The error message you may see is:

    error: This version of stackage.nix does not know
      about the Stackage resolver X.Y.

      You may need to update haskell.nix to one
      that includes a newer stackage.nix.

If updating the LTS, the compiler version may also have been
increased. Note that we maintain a patched GHC for cross-compiling to
Windows. **The GHC version that you use needs to be supported** by
`iohk-nix`. The error message in this case will be:

    error: This version of Nixpkgs does not contain GHC x.y.z

In both of these cases, update the `iohk-nix` revision to the latest
available using the instructions above.

### Adding new GHC versions

To add support for a new GHC version in `iohk-nix`, first update its
Nixpkgs version to one that provides that compiler, then work on
adding the version to [`nix-tools-iohk-extras.nix`](./nix-tools-iohk-extras.nix).

## Nix Gotchas

The [docs folder](./docs) contains a list of typical Nix errors
encountered while developing with Haskell and Nix.  The hope is
developers will add Nix related problems they've encountered and
solved to this folder.

* [Gotcha 1](./docs/gotcha-1.md) - attribute `unbuildable` missing (win32/unix dependency issues)
* [Gotcha 2](./docs/gotcha-2.md) - more help with build problems
* [Gotcha 3](./docs/gotcha-3.md) - filtering sources
* [Gotcha 4](./docs/gotcha-4.md) - overriding version pins
* [Gotcha 5](./docs/gotcha-5.md) - ghci
* [Gotcha 6](./docs/gotcha-6.md) - how to get longer error messages from `nix build`

## When making changes to `iohk-nix`

Please document any change that might affect project builds in the
[ChangeLog](./changelog.md). For example:

 - Bumping `nixpkgs` to a different branch.
 - Changing API (renaming attributes, changing function parameters, etc).
 - Bumping the `haskell.nix` version.

Also update the [`skeleton`](./skeleton/README.md) project if necessary.
