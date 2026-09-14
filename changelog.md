# ChangeLog

Please read these notes when updating your project's `iohk-nix`
version. There may have been changes which could break your build.

## 2026-09-13
  * **Breaking:** keys that cardano-config treats as removed are no longer
    emitted in any environment's `nodeConfig`, so node 11.2 does not warn about
    them on every parse.  `minNodeVersion` is now `11.2.0`.

    Dropped from every `environments.<env>.nodeConfig`:
      * `Protocol` - read `environments.<env>.consensusProtocol` instead.  It
        carries the same value and is present on older iohk-nix revisions too,
        where it is derived from the dropped key, so consumers can move to it
        before bumping their pin.  A consumer still reading
        `nodeConfig.Protocol` fails at evaluation with `attribute 'Protocol'
        missing`.
      * `MaxKnownMajorProtocolVersion` - mainnet only, and read by nothing in
        the node source.

    Additionally dropped from `testnet-template/config.json`:
      * `PBftSignatureThreshold`, `ApplicationName`, `ApplicationVersion`.

    `LastKnownBlockVersion-Major`, `-Minor` and `-Alt` are deliberately kept:
    cardano-config drops them but the node's own parser still requires the first
    two.

    Changed in `cardanoLib`:
      * `consensusProtocol` is now the literal `"Cardano"` rather than being
        derived from `networkConfig.Protocol`.  Cardano is the only consensus
        protocol still supported.

  * **Breaking:** `<env>-config.json` is now published in the cardano-config
    Version1 envelope for every environment, rather than the flat single-file
    form.  Still one config artifact per environment.

    **This requires a node whose cardano-config adapter maps
    `CheckpointsFile`.** Given an envelope the node skips its own parser and
    resolves with cardano-config alone, so an older node reads an enveloped
    mainnet or preview config with its checkpoints configuration silently empty.
    `minNodeVersion`, now `11.2.0`, is the contract that says which nodes are
    safe.

    New on every `environments.<env>`:
      * `configFormat` - `"enveloped"` (default) or `"flat"`, selecting the form
        published for that environment.  Set it to `"flat"` for an environment
        that must stay readable by an older node.
      * `nodeConfigEnveloped` - the enveloped form of `nodeConfig`, regardless
        of `configFormat`.

    Byron supported-protocol-version becomes a fixed 1/0/0 on the enveloped
    path, as cardano-config does not model `LastKnownBlockVersion-*`.  That is
    deliberate upstream and applies to every environment.

    `nodeConfig` itself is unchanged and remains flat.  Consumers reading it as
    a Nix value, including `dbSyncConfig` and `explorerConfig` which embed their
    own copy, are unaffected by `configFormat`.

  * Enveloped configs carry the `$schema` annotation cardano-config expects.
    Without it the node reports `MigratedToCurrentFormat` on every parse; with
    it the configuration is canonical and parses without warnings.  The URL
    points at the upstream `v1` tag, which tracks config format version 1 rather
    than the latest release, so it stays valid as cardano-config releases.

  * New `hydraJobs.cardano-config-lint`, built by `mkConfigLint`, fails if any
    environment or the testnet template carries a top-level node config key
    cardano-config will not resolve.  The recognised set is read from the
    cardano-config JSON schemas, so bumping that pin keeps the check current.
    This catches what the schemas cannot: neither sets `additionalProperties`,
    so a removed or misspelled key validates clean against them.

    New in `cardanoLib`: `mkConfigLint`, `lintTargets`, `mkEnvelope`,
    `propertyToSection`.

    `mkEnvelope` reproduces `cardano-config migrate` for any flat config, not
    just the ones shipped here, so it can be used on a hand-written config.  It
    applies the same renames (`EnableRpc`, `RpcSocketPath` and the
    `TargetNumberOf*` peer targets), drops the same removed and obsolete keys,
    collapses `ApplicationName` into `HermodTracing.TraceOptionNodeName`, and
    PascalCases the `AcceptedConnectionsLimit` sub-keys.  The one exception is a
    legacy *flat* `LedgerDB`, which `migrate` gathers into the nested form and
    this does not, since the configs here already emit the nested form.

    `mkConfigLint` reports a pre-rename key such as `TargetNumberOfRootPeers` as
    unrecognised rather than silently accepting it.  `migrate` would rewrite it,
    but the source is better fixed.

  * New source-only flake input `cardano-config`, pinned to
    `cardano-config-1.1.0.0`, matching the cardano-node 11.2 pin.  It supplies
    the JSON schemas the key-to-component mapping is built from.  Its own flake
    is deliberately not used as an input, as it pulls haskell.nix, hackage.nix,
    CHaP and iohk-nix itself.

## 2026-08-07
  * **Breaking:** legacy tracing (iohk-monitoring) config generation is removed,
    in line with cardano-node 11.1 dropping the legacy tracing system.
    `minNodeVersion` is now `11.1.0`.

    Removed from `cardanoLib`:
      * `defaultLogConfigLegacy` - use `defaultLogConfig`.
      * `mkEdgeTopology` - use `mkEdgeTopologyP2P`.  Legacy networking mode no
        longer exists; `mkTopology` is now unconditionally p2p.

    Removed from every `environments.<env>` attrset:
      * `nodeConfigLegacy` - use `nodeConfig`.  The generated
        `<env>-config-legacy.json` artifacts are no longer published.

    Changed in `cardanoLib`:
      * `submitApiConfig` is now the network-independent
        `defaultSubmitApiConfig` (new, also exported).  It carries only
        trace-dispatcher tracing options; the previous `GenesisHash` and
        `RequiresNetworkMagic` keys were unused by cardano-submit-api and are
        no longer emitted.  Consumers that read network identity out of
        `submitApiConfig` must take it from `nodeConfig` instead.
      * `explorer-log-config.nix` remains legacy iohk-monitoring format for
        db-sync and similar consumers.  It is not a valid tracing config for
        trace-dispatcher services.

  * `LedgerDB` snapshot options moved under a `LedgerDB.Snapshots` key and
    `SnapshotInterval` is now denominated in **slots**, not seconds, following
    the deterministic snapshot work in node 11.1.  Per-network values are set
    to `securityParam * 40`.

  * Added a `leios` environment for the Leios prototype network.

## 2026-05-04
  * Update blst to 0.3.15.
  * Add CI check for invalid `flake.lock` file.

## 2023-04-20
  * Added `blst` library with dynamic, and static lirbaries, as well as pkg-config metadata.
  * Renamed `blst` to `libblst`
  * Added [haskell-nix](https://github.com/input-output-hk/haskell.nix) `extraPkgconfigMappings` for `libblst` and `libsodium`.
    This should allow us to drop the previously needed hack to map `libsodium-vrf` to `libsodium`.

## 2021-07-22
  * Renamed `libsodium` to `libsodium-vrf` in the crypto overlay. This
    allows much more sharing from the binary caches.

    Any package which has dependencies on the VRF fork of libsodium
    must now add a Haskell.nix module to select the forked package.
    For example:

    ```nix
    ({ lib, pkgs, ...}: {
      # Use our forked libsodium from iohk-nix crypto overlay.
      packages.cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
      packages.cardano-crypto-class.components.library.pkgconfig = lib.mkForce [ [ pkgs.libsodium-vrf ] ];
    })
    ```

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
