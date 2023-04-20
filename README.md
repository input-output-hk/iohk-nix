# IOHK Common Nix Code

This repo contains build code and tools shared between IOHK projects.

1. Some util functions such as source filtering or helpers for [Haskell.nix](https://github.com/input-output-hk/haskell.nix).
2. Customized libraries (as an overlay for [nixpkgs](https://github.com/nixos/nixpkgs).
2. Nix builds of development tools such as cache-s3.

## When making changes to `iohk-nix`

Please document any change that might affect project builds in the
[ChangeLog](./changelog.md). For example:

 - Bumping `nixpkgs` to a different branch.
 - Changing API (renaming attributes, changing function parameters, etc).
