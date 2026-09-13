# Reshape a flat node config into the cardano-config Version1 envelope.
#
# This mirrors `cardano-config migrate`, so the output should match what that
# command produces from the same input, less the `$schema` annotation.  See
# `Cardano.Configuration.File.Migrate` in IntersectMBO/cardano-config.
#
# Node 11.2 still parses the config with its own POM parser, which reads flat
# keys only, so this form cannot yet be given to a node.  It is generated so the
# shape can be validated and handed to consumers ahead of POM being dropped.
{lib, cardanoConfigSrc}:
let
  inherit (builtins) attrNames elem filter fromJSON isAttrs isList listToAttrs map readFile;
  inherit (lib) concatMap foldl' optionalAttrs recursiveUpdate;

  # The component sections, matching `schemas/<section>.schema.json`.
  sections = [
    "ConsensusConfig"
    "LocalConnectionsConfig"
    "MempoolConfig"
    "NetworkConfig"
    "ProtocolConfig"
    "StorageConfig"
    "TestingConfig"
  ];

  schemaProperties = section:
    filter (k: k != "$schema")
      (attrNames (fromJSON (readFile "${cardanoConfigSrc}/schemas/${section}.schema.json")).properties);

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
  # mandatory for the node's POM parser, so they stay in the flat config and are
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
    RpcSocketPath = "GrpcSocketPath";
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
        present = attrNames value;
        collidingOld = filter (k: renamedKeys ? ${k} && elem renamedKeys.${k} present) present;
        kept = filter (k: !(elem k removedKeys) && !(elem k collidingOld)) present;
        rekey = k: {
          name = renamedKeys.${k} or k;
          value = scoped k (renameLegacy value.${k});
        };
      in
        listToAttrs (map rekey kept)
    else if isList value then map renameLegacy value
    else value;

  # Scoped fixups applied to a key's value after recursion.  Only the
  # AcceptedConnectionsLimit rename is mirrored: the `LedgerDB` fixups upstream
  # (`nestSnapshotOptions`, `nestBackend`) gather a legacy *flat* LedgerDB into
  # the nested form, and every config here already emits the nested form, so
  # they would be no-ops.  Revisit if a flat LedgerDB ever appears.
  scoped = key: value:
    if key == "AcceptedConnectionsLimit" && isAttrs value
    then listToAttrs (map (k: {
           name = acceptedConnectionsLimitKeys.${k} or k;
           value = value.${k};
         }) (attrNames value))
    else value;

  # Envelope annotations, lifted out of the config body.
  envelopeKeys = ["$schema" "Version" "MinNodeVersion" "Configuration"];

  # The format version this envelope declares, `currentFormatVersion` in
  # Schema.hs.  The schema gives no default to read it from.
  formatVersion = 1;

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
      configuration = foldl' (acc: key: recursiveUpdate acc (placeKey body key)) {} (attrNames body);
    in
      {
        Version = formatVersion;
        Configuration = configuration;
      }
      # Carried through when present, never invented.  Sits beside `Version`
      # rather than in a section so an SPO sees a too-old node immediately.
      // optionalAttrs (nodeConfig ? MinNodeVersion) {
        inherit (nodeConfig) MinNodeVersion;
      };

  # Every key cardano-config resolves out of a flat config.  A key outside this
  # set is either one of `removedKeys`, which migrate silently drops, or an
  # unrecognised key, which it keeps and warns about on every parse.  Both are
  # worth catching, so neither is included here.
  recognisedKeys = attrNames propertyToSection ++ tracingKeys ++ envelopeKeys;

  # Top-level keys of a flat node config that cardano-config would not resolve.
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
    inherit mkEnvelope propertyToSection recognisedKeys removedKeys renamedKeys
      tracingKeys tracingObsoleteKeys unrecognisedKeys;
  }
