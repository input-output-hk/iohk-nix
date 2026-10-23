# Reshape a legacy node config into the cardano-config envelope.
#
# This mirrors `cardano-config migrate`, so for the configs here the two produce
# the same output from the same input.  It is not a general equivalence:
# upstream warns where this drops silently, carries an existing `$schema` and
# `Version` through where this always stamps its own, and rejects a section that
# names a file rather than holding one.  See
# `Cardano.Configuration.File.Migrate` in IntersectMBO/cardano-config.
#
# Node 11.2 reads this form: given an envelope it skips its own POM parser and
# resolves with cardano-config alone.  That also means the envelope inherits
# cardano-config's adapter gaps, so not every environment can use it; see
# `configFormat` in default.nix.
#
# ## This whole module is transitional
#
# It exists because the legacy form is the source of truth and the envelope is
# derived from it.  That direction is forced, not chosen: `migrate` drops the
# keys in `removedKeys`, among them the LastKnownBlockVersion-* that POM
# requires, so the envelope is derivable from the legacy config but not the
# reverse.
#
# The end state is that nothing hands this a flat key.  At that point the
# translation below is dead and can go with it: `renameLegacy`, `placeKey`,
# `propertyToSection`, every hardcoded key table, the scoped fixups, and
# `mkConfigDrift` in default.nix which only exists to police those tables.  What
# survives is the envelope annotations, which something still has to stamp, and
# the lint, which validates keys against the schemas and is useful either way.
#
# Three things have to be true first, and none of them are yet:
#
#   * the node drops POM, the last reader of the legacy form's dropped keys.  It
#     requires `LastKnownBlockVersion-Major` and `-Minor` with `.:`, and only
#     ever sees the legacy dialect
#   * no consumer still takes the legacy `nodeConfig` as a file, which
#     `mkExplorerConfig` currently does for the explorer
#   * every caller of the nixos service writes envelope shape, and the service
#     stops synthesising flat keys of its own.  It cannot do the latter while any
#     consumer still sets `configFormat = "legacy"`, since POM reads the flat
#     spelling and only that; see `baseInstanceConfig` in
#     cardano-node-service.nix for the mechanics.  Writing each
#     `<env>-config.nix` in envelope shape is not enough on its own either: the
#     service envelopes the assembled instance config, so a flat `nodeConfig`,
#     `extraNodeConfig` or `extraNodeInstanceConfig` from any consumer keeps the
#     translation alive.
#     Them staying flat is a decision rather than an oversight, it is what keeps
#     ogmios, tx-centrifuge and cardano-parts working unchanged, so expect this
#     to be the last of the four to come true
#
# So treat the brittleness below as a cost of the transition rather than
# something to invest in hardening.
{lib, cardanoConfigSrc}:
let
  inherit (builtins) attrNames elemAt elem filter fromJSON isAttrs isList isString listToAttrs
    map match split
    readFile typeOf;
  inherit (lib) concatMap foldl' optionalAttrs recursiveUpdate;

  # As of cardano-config 2.0.0.0 there is one schema file.  Its top level is the
  # envelope annotations alone; every component section sits under
  # `Configuration` with its properties inlined.  Before 2.0.0.0 the sections
  # were also top-level properties and each had its own
  # `schemas/<name>.schema.json`, which is what this used to read.
  configSchema = fromJSON (readFile "${cardanoConfigSrc}/schemas/config.schema.json");

  sectionSchemas = configSchema.properties.Configuration.properties;

  # The component sections.  `HermodTracing` sits alongside them but carries no
  # properties, because cardano-config describes it only as a path or an object
  # and leaves the shape to trace-dispatcher.  So "has properties" is what
  # separates a component from the rest, and an added or renamed section is
  # picked up by a pin bump rather than silently ignored.
  sections = filter (name: sectionSchemas.${name} ? properties) (attrNames sectionSchemas);

  schemaProperties = section: attrNames sectionSchemas.${section}.properties;

  # Every component property name mapped to the section that owns it.  Read
  # from the schemas rather than restated here, so bumping the cardano-config
  # pin picks up added or moved keys.  Each property belongs to exactly one
  # section, which the assert below pins.
  propertyToSection = listToAttrs (concatMap
    (section: map (property: {name = property; value = section;}) (schemaProperties section))
    sections);

  propertyCount = foldl' (acc: section: acc + (builtins.length (schemaProperties section))) 0 sections;

  # The flat trace-dispatcher keys that belong under `HermodTracing`.  These
  # are not in any schema: cardano-config describes HermodTracing only as a
  # path or an object, leaving the shape to trace-dispatcher.  Taken from
  # `tracingLegacyKeys` in Migrate.hs.
  tracingKeys = [
    "TraceOptions"
    "TraceOptionForwarder"
    "TraceOptionNodeName"
    "TraceOptionMetricsPrefix"
    "TraceOptionResourceFrequency"
    "TraceOptionLedgerMetricsFrequency"
    "TracePrometheusSimpleRun"
  ];

  # Keys cardano-config no longer parses, dropped rather than carried forward as
  # perpetual unrecognised key warnings.  Taken from `removedFields` in
  # Migrate.hs.  Note `LastKnownBlockVersion-Major` and `-Minor` remain
  # mandatory for the node's POM parser, so they stay in the legacy config and are
  # dropped only here.
  removedKeys = [
    "PBftSignatureThreshold"
    "LastKnownBlockVersion-Major"
    "LastKnownBlockVersion-Minor"
    "LastKnownBlockVersion-Alt"
    "ApplicationVersion"
    "EnableP2P"
    "Protocol"
    "MaxKnownMajorProtocolVersion"
  ];

  # Obsolete iohk-monitoring keys, dropped outright.  Unlike `removedKeys` these
  # go only at the top level.  Taken from `tracingObsoleteKeys` in Migrate.hs.
  tracingObsoleteKeys = [
    "UseTraceDispatcher"
    "TurnOnLogging"
    "TurnOnLogMetrics"
    "defaultBackends"
    "defaultScribes"
    "setupBackends"
    "setupScribes"
    "minSeverity"
    "options"
  ];

  # Keys renamed in the current naming series, as old -> new.  cardano-config
  # only accepts the new names.  Taken from `renamedFields` in Migrate.hs.
  # Matching is on the whole key, so `SyncTargetNumberOfRootPeers` is untouched
  # by the `TargetNumberOf*` entries.
  renamedKeys = {
    EnableRpc = "EnableGrpc";
    MempoolCapacityBytesOverride = "CapacityBytesOverride";
    RpcSocketPath = "GrpcSocketPath";
    RpcListenAddress = "GrpcListenAddress";
    RpcListenPort = "GrpcListenPort";
    RpcTlsCertificateFile = "GrpcTlsCertificateFile";
    RpcTlsPrivateKeyFile = "GrpcTlsPrivateKeyFile";
    RpcTlsChainCertificateFiles = "GrpcTlsChainCertificateFiles";
    TargetNumberOfRootPeers = "DeadlineTargetNumberOfRootPeers";
    TargetNumberOfKnownPeers = "DeadlineTargetNumberOfKnownPeers";
    TargetNumberOfEstablishedPeers = "DeadlineTargetNumberOfEstablishedPeers";
    TargetNumberOfActivePeers = "DeadlineTargetNumberOfActivePeers";
    TargetNumberOfKnownBigLedgerPeers = "DeadlineTargetNumberOfKnownBigLedgerPeers";
    TargetNumberOfEstablishedBigLedgerPeers = "DeadlineTargetNumberOfEstablishedBigLedgerPeers";
    TargetNumberOfActiveBigLedgerPeers = "DeadlineTargetNumberOfActiveBigLedgerPeers";
  };

  # The generic sub-keys of an `AcceptedConnectionsLimit` object.  Too generic to
  # rewrite unconditionally, so cardano-config scopes them to that key.  Taken
  # from `acceptedConnectionsLimitFields` in Migrate.hs.
  acceptedConnectionsLimitKeys = {
    hardLimit = "HardLimit";
    softLimit = "SoftLimit";
    delay = "Delay";
  };

  # The sections that carried a `Config` suffix before cardano-config 2.0.0.0,
  # as old -> new.  Renamed at the `Configuration` level only, since the current
  # names are too generic to rewrite at depth.  Taken from `sectionRenames` in
  # Migrate.hs.
  sectionRenames = {
    ConsensusConfig = "Consensus";
    LocalConnectionsConfig = "LocalConnections";
    MempoolConfig = "Mempool";
    NetworkConfig = "Network";
    ProtocolConfig = "Protocol";
    StorageConfig = "Storage";
    TestingConfig = "Testing";
  };

  # The `LedgerDB.Snapshots` renames, scoped to that object upstream.  Taken
  # from `snapshotOptionFields` in Migrate.hs, new in cardano-config 2.0.0.0.
  snapshotRenamedKeys = {
    SnapshotInterval = "Interval";
    SlotOffset = "Offset";
  };

  # The snapshot options a legacy config may write flat under `LedgerDB`, which
  # cardano-config reads only from a nested `Snapshots` object.  These are the
  # pre-rename spellings; `snapshotRenamedKeys` rewrites two of them afterwards.
  # Taken from `snapshotOptionKeys` in Migrate.hs.
  snapshotFlatKeys = [
    "SnapshotInterval"
    "SlotOffset"
    "RateLimit"
    "MinDelay"
    "MaxDelay"
    "NumOfDiskSnapshots"
  ];

  # Both names of the mempool capacity override.  A `"NoOverride"` value under
  # either is dropped rather than carried forward, see `renameLegacy`.  Taken
  # from `mempoolCapacityOverrideKeys` in Migrate.hs.
  mempoolCapacityOverrideKeys = [
    "MempoolCapacityBytesOverride"
    "CapacityBytesOverride"
  ];

  # Rewrite one object's keys through a rename table.  Where both names are
  # present the new one wins, as upstream does.
  renameIn = table: obj:
    let
      present = attrNames obj;
      collidingOld = filter (k: table ? ${k} && elem table.${k} present) present;
      kept = filter (k: !(elem k collidingOld)) present;
    in
      listToAttrs (map (k: {name = table.${k} or k; value = obj.${k};}) kept);

  # `renameLegacy` from Migrate.hs: drop removed keys and rewrite renamed ones at
  # any depth, recursing through objects and arrays alike.  Runs before the
  # regrouping below, exactly as `migrate = reshape . renameLegacy` does.
  #
  # Where both the old and the new name are present at the same level the new
  # name wins, matching the `RenamedKeyCollision` branch upstream.  We drop the
  # old one silently; cardano-config warns.
  renameLegacy = value:
    if isAttrs value then
      let
        # A mempool capacity override of "NoOverride" means the same as omitting
        # the key, which is now the only way to say it, so upstream drops the
        # entry under either name.  Dropped here before the collision check, as
        # it is upstream, or a "NoOverride" under the old name would rename onto
        # a real value under the new one.
        isNoOverride = k:
          elem k mempoolCapacityOverrideKeys && value.${k} == "NoOverride";
        present = filter (k: !(isNoOverride k)) (attrNames value);
        collidingOld = filter (k: renamedKeys ? ${k} && elem renamedKeys.${k} present) present;

        # `Protocol` is both a removed key, the legacy protocol selector, which is
        # a string, and the current name of a section, which is an object, so
        # only the former is dropped.  Reachable once a caller writes part of the
        # config in envelope shape, see `mkEnvelope`.
        isRemovedKey = k: elem k removedKeys && !(k == "Protocol" && isAttrs value.${k});

        kept = filter (k: !(isRemovedKey k) && !(elem k collidingOld)) present;
        rekey = k: {
          name = renamedKeys.${k} or k;
          value = scoped k (renameLegacy value.${k});
        };
      in
        listToAttrs (map rekey kept)
    else if isList value then map renameLegacy value
    else value;

  # Gather the flat snapshot options under `Snapshots`, the only form
  # cardano-config's LedgerDB codec reads.  An existing `Snapshots` wins and the
  # flat keys are dropped rather than merged, since it is an explicit policy and
  # may be the string `"Mithril"`.  Mirrors `nestSnapshotOptions` in Migrate.hs.
  nestSnapshotOptions = ledgerDb:
    let
      flat = filter (k: elem k snapshotFlatKeys) (attrNames ledgerDb);
      rest = removeAttrs ledgerDb flat;
    in
      if flat == [] then ledgerDb
      else if rest ? Snapshots then rest
      else rest // {Snapshots = listToAttrs (map (k: {name = k; value = ledgerDb.${k};}) flat);};

  # Rewrite the renamed snapshot options, which only applies to an options
  # object.  A `"Mithril"` policy is left alone.  Mirrors
  # `renameSnapshotOptions` in Migrate.hs.
  renameSnapshotOptions = ledgerDb:
    if ledgerDb ? Snapshots && isAttrs ledgerDb.Snapshots
    then ledgerDb // {Snapshots = renameIn snapshotRenamedKeys ledgerDb.Snapshots;}
    else ledgerDb;

  # Fold the flat `V2LSM` backend into the tagged form the LedgerDB codec reads.
  # `V2InMemory` is a plain string on both sides and is left alone.  Mirrors
  # `nestBackend` in Migrate.hs.
  nestBackend = ledgerDb:
    if (ledgerDb.Backend or null) == "V2LSM"
    then
      removeAttrs ledgerDb ["LSMDatabasePath" "LSMExportPath"]
      // {
        Backend.LSM =
          optionalAttrs (ledgerDb ? LSMDatabasePath) {DatabasePath = ledgerDb.LSMDatabasePath;}
          // optionalAttrs (ledgerDb ? LSMExportPath) {ExportPath = ledgerDb.LSMExportPath;};
      }
    else ledgerDb;

  # Scoped fixups, mirroring the upstream rewrites that apply under a parent key
  # rather than at any depth.
  #
  # The three LedgerDB rewrites run in upstream's order: nest, then rename, since
  # the rename only looks at an existing `Snapshots` object, then fold the
  # backend.  The shipped env configs are already nested, but the nixos service
  # assembles the config it envelopes and writes `Backend = "V2LSM"` with a
  # sibling `LSMDatabasePath`, so a host reaches these even when no environment
  # does.  Omitting them costs no setting, since cardano-config reads both
  # shapes, but leaves the file non-canonical and the node warns
  # `MigratedToCurrentFormat` on every start.
  scoped = key: value:
    if key == "AcceptedConnectionsLimit" && isAttrs value
    then renameIn acceptedConnectionsLimitKeys value
    else if key == "LedgerDB" && isAttrs value
    then nestBackend (renameSnapshotOptions (nestSnapshotOptions value))
    else value;

  # Envelope annotations, lifted out of the config body.  Since 2.0.0.0 the
  # schema's top level is exactly these, the body having moved under
  # `Configuration`, so they are read off directly.
  envelopeKeys = attrNames configSchema.properties;

  # The format version this envelope declares, `currentFormatVersion` in
  # Schema.hs.  The schema only constrains it to `minimum: 1`, so read it from
  # the source; `mkConfigDrift` in default.nix fails the build if this and the
  # pin disagree.
  formatVersion = 2;

  # The annotation `migrate` stamps.  Emitting it is what makes a config
  # canonical: cardano-config warns `MigratedToCurrentFormat` whenever migrate
  # changes the document, and without `$schema` adding it is the one change it
  # makes.  With it, migrate is a no-op and the node parses warning free.
  #
  # The `vX` tag tracks the format version, not the release: upstream cuts one
  # per major and the schemas cannot change without bumping it, so the tag is
  # derived from `formatVersion` rather than written twice.  See the versioning
  # section of the cardano-config README.
  schemaUrl =
    "https://raw.githubusercontent.com/IntersectMBO/cardano-config"
    + "/v${toString formatVersion}/schemas/config.schema.json";

  # Where a flat key lands inside `Configuration`, mirroring `place` in
  # Migrate.hs and keeping its branch order.  An unrecognised key is kept at the
  # top of `Configuration` rather than dropped, matching migrate, so nothing is
  # silently lost.  It still warns on the next parse.
  #
  # `removedKeys` is not tested here: `renameLegacy` has already dropped those at
  # every depth, which is where migrate does it.
  placeKey = body: key:
    let
      value = body.${key};
      nestAs = section: targetKey: {${section} = {${targetKey} = value;};};
      nest = section: nestAs section key;
    in
      if elem key tracingObsoleteKeys then {}
      else if elem key tracingKeys then nest "HermodTracing"
      # The obsolete Byron software-version name is repurposed as the tracing
      # node name, since nothing else reads it.
      else if key == "ApplicationName" then nestAs "HermodTracing" "TraceOptionNodeName"
      else if propertyToSection ? ${key} then nest propertyToSection.${key}
      else {${key} = value;};

  mkEnvelope = nodeConfig:
    let
      renamed = renameLegacy nodeConfig;
      body = removeAttrs renamed envelopeKeys;
      regrouped = foldl' (acc: key: recursiveUpdate acc (placeKey body key)) {} (attrNames body);

      # A caller may already write part of the config in envelope shape, which
      # is where this is all going once POM is dropped.  Keep that body and
      # regroup the flat siblings into it, rather than dropping it with the
      # other envelope annotations; the enveloped value wins a collision, as
      # upstream does.  `renameLegacy` has already recursed through it, so the
      # renames and the scoped fixups apply at depth either way.
      #
      # A `Configuration` naming a file rather than holding one is rejected
      # here, as upstream rejects a section that does, rather than failing
      # inside `recursiveUpdate` against a value that is not a set.
      #
      # A body written against cardano-config before 2.0.0.0 still names its
      # sections with the `Config` suffix, so rename them here, at the only
      # level sections live at, before the regrouped flat keys merge in under
      # the current names.
      envelopeBody =
        if !(renamed ? Configuration)
        then {}
        else if isAttrs renamed.Configuration
        then renameIn sectionRenames renamed.Configuration
        else throw ("cardanoLib: the node config's `Configuration` is a "
          + "${typeOf renamed.Configuration} rather than a configuration object."
          + "  It holds the configuration directly, so copy the contents of the"
          + " file it names in under it.");

      configuration = recursiveUpdate regrouped envelopeBody;
    in
      {
        "$schema" = schemaUrl;
        Version = formatVersion;
        Configuration = configuration;
      }
      # Carried through when present, never invented.  Sits beside `Version`
      # rather than in a section so an SPO sees a too-old node immediately.
      // optionalAttrs (nodeConfig ? MinNodeVersion) {
        inherit (nodeConfig) MinNodeVersion;
      };

  # The same values as cardano-config holds them, extracted from the pinned
  # source so `mkConfigDrift` in default.nix can fail the build when a pin bump
  # changes one.  `propertyToSection`, `sections` and `envelopeKeys` are derived
  # from the JSON schemas and so need no check; everything below lives only as a
  # Haskell literal.
  #
  # Parsing is line based against the upstream formatting.  An upstream reformat
  # therefore breaks the check rather than letting drift through, which is the
  # right way round but is a real maintenance cost.
  #
  # This is the most temporary code here, and the least worth hardening: it
  # exists only to police tables that exist only because the envelope is derived
  # from a legacy source.  Writing the configs in envelope shape deletes the
  # tables, and this with them.  See the module header.
  upstream = let
    migrateHs = readFile "${cardanoConfigSrc}/src/Cardano/Configuration/File/Migrate.hs";
    schemaHs = readFile "${cardanoConfigSrc}/src/Cardano/Configuration/Schema.hs";

    sourceLines = text: filter isString (split "\n" text);

    # The lines of a `<name> =\n  [ .. ]` literal, brackets excluded.
    listBody = text: name:
      (foldl'
        (acc: l:
          if acc.done then acc
          else if !acc.started then acc // {started = l == "${name} =";}
          else if l == "  ]" then acc // {done = true;}
          else acc // {out = acc.out ++ [l];})
        {started = false; done = false; out = [];}
        (sourceLines text)).out;

    # Every quoted string of a `<name> = ["a", "b"]` literal written on one
    # line.  Upstream writes the short lists that way, and `listBody` only sees
    # the multi-line form, so a one-liner would silently scrape as empty.
    # Returns null when there is no such literal, to tell "not inline" apart
    # from "inline and empty".
    inlineStrings = text: name:
      let m = match ".*\n${name} = (\\[[^\n]*)\n.*" text;
      in if m == null
         then null
         else concatMap (x: if isList x then x else []) (split "\"([^\"]+)\"" (elemAt m 0));

    # One quoted string per line, for a `[Text]` literal.
    strings = text: name:
      let inline = inlineStrings text name;
      in if inline != null
         then inline
         else concatMap
           (l: let m = match "[^\"]*\"([^\"]+)\".*" l; in if m == null then [] else m)
           (listBody text name);

    # Two quoted strings per line, for a `[(Text, Text)]` rename table.
    renames = text: name:
      listToAttrs (concatMap
        (l:
          let m = match "[^\"]*\"([^\"]+)\", *\"([^\"]+)\".*" l;
          in if m == null then [] else [{name = elemAt m 0; value = elemAt m 1;}])
        (listBody text name));

    intDef = text: name:
      let m = match ".*\n${name} = ([0-9]+)\n.*" text;
      in if m == null then null else fromJSON (elemAt m 0);
  in {
    removedKeys = strings migrateHs "removedFields";
    tracingKeys = strings migrateHs "tracingLegacyKeys";
    tracingObsoleteKeys = strings migrateHs "tracingObsoleteKeys";
    renamedKeys = renames migrateHs "renamedFields";
    acceptedConnectionsLimitKeys = renames migrateHs "acceptedConnectionsLimitFields";
    snapshotRenamedKeys = renames migrateHs "snapshotOptionFields";
    sectionRenames = renames migrateHs "sectionRenames";
    snapshotFlatKeys = strings migrateHs "snapshotOptionKeys";
    mempoolCapacityOverrideKeys = strings migrateHs "mempoolCapacityOverrideKeys";
    formatVersion = intDef schemaHs "currentFormatVersion";
  };

  # What the checker compares: our value against the pin's, per name.
  driftPairs = {
    inherit removedKeys tracingKeys tracingObsoleteKeys renamedKeys snapshotRenamedKeys
      snapshotFlatKeys sectionRenames mempoolCapacityOverrideKeys
      acceptedConnectionsLimitKeys formatVersion;
  };

  # Every key cardano-config resolves out of a legacy config.  A key outside this
  # set is either one of `removedKeys`, which migrate silently drops, or an
  # unrecognised key, which it keeps and warns about on every parse.  Both are
  # worth catching, so neither is included here.
  recognisedKeys = attrNames propertyToSection ++ tracingKeys ++ envelopeKeys;

  # Top-level keys of a legacy node config that cardano-config would not resolve.
  # `allowed` carries the ones a caller keeps on purpose.
  #
  # Reported against the key as written, not the renamed one, so a config still
  # using an old name is flagged rather than silently accepted.  `renamedKeys`
  # names are therefore deliberately absent from `recognisedKeys`: migrate would
  # rewrite them, but we would rather fix the source.  An obsolete tracing key is
  # reported for the same reason, since migrate drops it outright.
  unrecognisedKeys = allowed: nodeConfig:
    filter (k: !(elem k (recognisedKeys ++ allowed))) (attrNames nodeConfig);

in
  assert (builtins.length (attrNames propertyToSection)) == propertyCount;
  {
    inherit driftPairs mkEnvelope propertyToSection recognisedKeys removedKeys
      renamedKeys schemaUrl sections tracingKeys tracingObsoleteKeys
      unrecognisedKeys upstream;
  }
