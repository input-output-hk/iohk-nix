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


## How to use in your project

Use `iohk-nix` by "pinning" its git revision and source hash in a JSON
file. Then use iohk-nix to get nixpkgs. This is usually done with the
default arguments to `default.nix`. For example:

```nix
# default.nix
{ config ? {}
, system ? builtins.currentSystem
, iohkLib ? import ./nix/iohk-common.nix { inherit config system; }
, pkgs ? iohkLib.pkgs
}:

{
  # your builds go here
}
```

The `config` and `system` arguments above are needed when building for
other systems. They have default values, and should be passed through
to `iohk-nix`.

Now set up `./nix/iohk-common.nix`, which is pure boilerplate:

```nix
let
  spec = builtins.fromJSON (builtins.readFile ./iohk-nix-src.json);
in import (builtins.fetchTarball {
  url = "${spec.url}/archive/${spec.rev}.tar.gz";
  inherit (spec) sha256;
})
```

And create `iohk-nix-src.json`. You will need `nix-prefetch-git` (get it
with `nix-env -iA` or `nix-shell -p`). The `--rev` option defaults to
the HEAD of the `master` branch.

```
$ nix-prefetch-git https://github.com/input-output-hk/iohk-nix [ --rev master ] | tee ./nix/iohk-nix-src.json
```

Alternatively, the JSON file can be downloaded from [Buildkite](https://buildkite.com/input-output-hk/iohk-nix/builds/latest?branch=master).

## How to update the `iohk-nix` revision.

To get the latest version of `iohk-nix`, update the `iohk-nix-src.json` file:

```
$ nix-prefetch-git https://github.com/input-output-hk/iohk-nix | tee ./nix/iohk-nix-src.json
```

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

The [gotchas folder](./gotchas) is dedicated to typical Nix errors encountered while developing with Haskell and Nix.
The hope is developers will add Nix related problems they've encountered and solved to this folder.

* [Gotcha 1](./gotchas/gotcha-1.md) - attribute `unbuildable` missing (win32/unix dependency issues)
* [Gotcha 2](./gotchas/gotcha-2.md) - more help with build problems
* [Gotcha 3](./gotchas/gotcha-3.md) - filtering sources
* [Gotcha 4](./gotchas/gotcha-4.md) - overriding version pins


## When making changes to `iohk-nix`

Please document any change that might affect project builds in the
[ChangeLog](./changelog.md). For example:

 - Bumping `nixpkgs` to a different branch.
 - Changing API (renaming attributes, changing function parameters, etc).
 - Bumping the `haskell.nix` version.
