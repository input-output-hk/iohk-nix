# IOHK Skeleton Project

[![Build status](https://badge.buildkite.com/e5b12d0fd507084fbdb1849da2de467f1de66b3e5c6d954554.svg)](https://buildkite.com/input-output-hk/iohk-nix)

This is an empty reference project which shows how to set up CI with
Hydra and Buildkite, using the `iohk-nix` library.


## Contents

 * `iohk-skeleton.cabal`, `stack.yaml`, `app/`, `bench/`, `src/`, `test/` — The example Haskell package.
 * [`.buildkite/`](./buildkite/) — Buildkite pipeline definitions and related scripts.
 * [`.gitattributes`](./.gitattributes) — Metadata for GitHub so that it ignores auto-generated files in diffs.
 * [`default.nix`](./default.nix) — The main Nix build file.
 * [`release.nix`](./release.nix) — The Hydra release jobset.
 * [`shell.nix`](./shell.nix) — Default `nix-shell` development environment.
 * [`docs/`](./docs/) — Example Nix build of a LaTeX document.
 * [`nix/`](./nix/) — The proper location for all Nix code and files, apart from `{default,release,shell}.nix`.
 * [`nix/.stack.nix/`](./nix/.stack.nix/) — Auto-generated Nix expressions. These need to be checked in to Git, but don't edit them manually.
 * [`nix/pkgs.nix`](./nix/pkgs.nix) — The Haskell package set.

## How to set up a project

1. Copy this directory to a new location and then:
   ```
   git init
   git commit --allow-empty -m "Initial commit"
   git add .
   git commit -m "Copy iohk-skeleton"
   ```

2. Run `./nix/update-iohk-nix.sh` to update the iohk-nix pin.

3. Find all instances of `iohk-skeleton` or `iohkSkeleton` and replace
   them with your project name. Find all instances of `TODO` and
   address them. You can use the following commands to do that:

   ```
   git grep -i 'iohk.\?skeleton'
   nix-shell --run "lentil -T fixme ."
   ```

4. Some of the `TODO` comments will say "remove the line below and uncomment the
   next line." These are there just so that the `skeleton` project can be built
   in CI within `iohk-nix`.

   The config files should also have example code and comments with
   instructions. Have a skim through these.

5. When finished, replace this file with your own, and delete
   redundant comments from the config files.

6. Set up the GitHub repo, Buildkite pipelines, and Hydra jobset according to
   the instructions in [`docs/start.md`](../docs/start.md).

7. If something in this project needs to be improved, please
   [open a PR](https://github.com/input-output-hk/iohk-nix/pulls).
   If you have any other problems, ask in
   [#devops-crossteam-ci](https://app.slack.com/client/T0N639Z4N/CAP8NM7N0).


## Testing the build locally

1. Test the Nix build.

   ```
   nix-build --keep-going
   ```

   This will build all attributes of `default.nix`.

2. Test the Hydra jobset. You can't simply build everything, because some of
   these jobs will be for other platforms. Replace `iohk-skeleton` with your
   project's name in this command:

   ```
   nix-build release.nix -A native.iohk-skeleton.x86_64-linux
   ```

3. Test Buildkite. Unfortunately, there is no easy way to test a Buildkite
   pipeline locally, other than copying & pasting the commands into your shell.


## To be documented

- How to get links to PDF documentation builds in Hydra.
- How to update a website in CI (e.g. `gh-pages`).
- How to get Hackage documentation for the project.
