# ChangeLog

Please read these notes when updating your project's `iohk-nix`
version. There may have been changes which could break your build.

## 2026-10-06
  * **Breaking:** `mainnet` now defaults to `ConsensusMode: GenesisMode`, as
    preprod, preview, sanchonet and dijkstra already did.  leios stays on
    `PraosMode`.

    Genesis mode requires a peer snapshot: the topology must declare
    `peerSnapshotFile`, and unlike PraosMode a missing snapshot is fatal rather
    than tolerated.  `mkTopology` declares it for every network and
    `mainnet-peer-snapshot.json` is published alongside the config, so consumers
    taking both from here need no change.  A hand-assembled topology, or one
    carried over from before the snapshot was declared, will not start.

  * **Breaking:** keys that cardano-config treats as removed are no longer
    emitted in any environment's `nodeConfig`, so node 11.2 does not warn about
    them on every parse.  `minNodeVersion` is now `11.2.0`.

    Dropped from every `environments.<env>.nodeConfig`:
      * `MaxKnownMajorProtocolVersion` - mainnet only, and read by nothing in
        the node source.

    Additionally dropped from `testnet-template/config.json`:
      * `PBftSignatureThreshold`, `ApplicationName`, `ApplicationVersion`.

    `Protocol` and `LastKnownBlockVersion-Major`, `-Minor`, `-Alt` are
    deliberately kept.  cardano-config drops all four, so they are absent from
    the enveloped config, but the flat `nodeConfig` has two readers that require
    them: the node's own POM parser, which needs the first two block-version
    keys, and db-sync, which reads `Protocol` and all three block-version keys
    as mandatory in `Cardano/DbSync/Config/Node.hs`.  They can go once db-sync
    reads an envelope.

    Changed in `cardanoLib`:
      * `consensusProtocol` is now the literal `"Cardano"` rather than being
        derived from `networkConfig.Protocol`.  Cardano is the only consensus
        protocol still supported, and the key it was derived from goes away
        once db-sync reads an envelope.

  * **Breaking:** `<env>-config.json` is now published in the cardano-config
    envelope for every environment, rather than the flat single-file form.
    Still one config artifact per environment.

    **This requires a node whose cardano-config adapter maps
    `CheckpointsFile`.** Given an envelope the node skips its own parser and
    resolves with cardano-config alone, so an older node reads an enveloped
    mainnet or preview config with its checkpoints configuration silently empty.
    `minNodeVersion`, now `11.2.0`, is the contract that says which nodes are
    safe.

    New on every `environments.<env>`:
      * `configFormat` - `"enveloped"` (default) or `"legacy"`, selecting the
        dialect published for that environment.  The names match the node's own
        `ConfigurationDialect`.
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
    points at the upstream `vN` tag for the config format version, currently
    `v2`, rather than at a release.

  * New `hydraJobs.cardano-config-lint`, built by `mkConfigLint`, fails if any
    environment or the testnet template carries a top-level node config key
    cardano-config will not resolve.  The recognised set is read from the
    cardano-config JSON schema, so bumping that pin keeps the check current.
    This catches what the schema cannot: it does not set
    `additionalProperties`, so a removed or misspelled key validates clean
    against it.

  * New `hydraJobs.cardano-config-drift`, built by `mkConfigDrift`, fails if
    `envelope.nix` and the pinned cardano-config disagree about any value that
    cannot be derived from the JSON schemas: the rename and drop tables and the
    format version, which exist only as Haskell literals and so are restated in
    Nix.  `removedFields` alone gained two entries between cardano-config
    1.0.0.0 and 1.1.0.0, so this is drift that happens in practice.

    The section list and the envelope annotations are now derived from
    `config.schema.json` rather than restated, so an added or renamed section
    arrives with a pin bump instead of being silently ignored, and the `$schema`
    URL's version tag is derived from the format version.

    This, `cardano-config-lint` and `cardano-config-schema` all write their
    result to `$out` as JSON on success rather than an empty file, so a green
    job records what it checked and two revisions can be diffed to see what
    moved.

    None of them covers the behavioural parts of `migrate`, the
    `ApplicationName` collapse and the deliberately omitted flat `LedgerDB`
    fixups.  Only comparing `mkEnvelope` output against real
    `cardano-config migrate` output covers those, which needs a built binary and
    so belongs downstream.

    New in `cardanoLib`: `mkConfigLint`, `lintTargets`, `mkConfigDrift`,
    `mkConfigSchema`, `mkEnvelope`, `propertyToSection`.

    `mkEnvelope` reproduces `cardano-config migrate` for any flat config, not
    just the ones shipped here, so it can be used on a hand-written config.  It
    applies the same renames (the `Rpc*` keys to their `Grpc*` spellings, the
    `TargetNumberOf*` peer targets to their `Deadline` prefixed names, and
    `MempoolCapacityBytesOverride` to `CapacityBytesOverride`), drops the same
    removed and obsolete keys,
    collapses `ApplicationName` into `HermodTracing.TraceOptionNodeName`, and
    PascalCases the `AcceptedConnectionsLimit` sub-keys.  The one exception is a
    legacy *flat* `LedgerDB`, which `migrate` gathers into the nested form and
    this does not, since the configs here already emit the nested form.

    `mkConfigLint` reports a pre-rename key such as `TargetNumberOfRootPeers` as
    unrecognised rather than silently accepting it.  `migrate` would rewrite it,
    but the source is better fixed.

  * New `hydraJobs.cardano-config-schema`, built by `mkConfigSchema`, validates
    every published `<env>-config.json` against `config.schema.json` from the
    cardano-config pin.  It reads the artifact `mkConfigHtml` writes rather than
    the Nix attrset, so what is checked is the file an operator downloads,
    genesis paths and all.  Type and enum errors are caught at any depth: a
    `Backend` of `V1LMDB` three levels inside `Storage` fails the build.

    Only the enveloped dialect is covered.  The pin ships one schema, for the
    envelope; the legacy one-file schema cardano-config 1.x carried is gone, so
    a `configFormat = "legacy"` environment is named as skipped in the job
    output rather than passed over in silence.  The testnet template is not a
    target either, as it names genesis files without the hashes the schema
    requires and is never published.

    It also fails if the schema's own `$id` and the `$schema` URL `envelope.nix`
    stamps into every config disagree.  That is the one way a pin bump could
    leave published configs claiming conformance to a schema the validation
    never read.

  * New source-only flake input `cardano-config`, pinned to
    `cardano-config-2.1.0.0`.  That is the release cardano-node resolves to: it
    takes cardano-config from CHaP bounded `^>= 2.1`, which is
    `>= 2.1 && < 2.2`, so 2.1.0.0 even though 2.2.x is published.  Follow the
    node when bumping it, so both sides read the same schema.

    It supplies the JSON schema the key-to-component mapping is built from.  Its
    own flake is deliberately not used as an input, as it pulls haskell.nix,
    hackage.nix, CHaP and iohk-nix itself.

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
