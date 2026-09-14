# Reshape a flat node config into the cardano-config Version1 envelope.
#
# This mirrors `cardano-config migrate`, so the output matches what that command
# produces from the same input.  See `Cardano.Configuration.File.Migrate` in
# IntersectMBO/cardano-config.
#
# Node 11.2 reads this form: given an envelope it skips its own POM parser and
# resolves with cardano-config alone.  That also means the envelope inherits
# cardano-config's adapter gaps, so not every environment can use it; see
# `configFormat` in default.nix.
#
# ## This whole module is transitional
#
# It exists because the flat form is the source of truth and the envelope is
# derived from it.  That direction is forced, not chosen: `migrate` drops the
# keys in `removedKeys`, among them the LastKnownBlockVersion-* that POM
# requires, so the envelope is derivable from the flat config but not the
# reverse.
#
# The end state is to write each `<env>-config.nix` in envelope shape directly.
# At that point the translation below is dead and can go with it: `renameLegacy`,
# `placeKey`, `propertyToSection`, every hardcoded key table, the scoped fixups,
# and `mkConfigDrift` in default.nix which only exists to police those tables.
# What survives is the lint, which validates keys against the schemas and is
# useful either way.
#
# Three things have to be true first, and none of them are yet:
#
#   * the node drops POM, so nothing needs the flat form's dropped keys
#   * cardano-api reads the envelope, so db-sync and friends can be pointed at
#     one (`readNodeConfig` is flat-only today)
#   * no consumer still takes the flat `nodeConfig` as a file, which
#     `mkExplorerConfig` currently does for db-sync and the explorer
#
# So treat the brittleness below as a cost of the transition rather than
# something to invest in hardening.
{lib, cardanoConfigSrc}:
let
  inherit (builtins) attrNames elemAt elem filter fromJSON isAttrs isList isString listToAttrs
    map match pathExists split
    readFile;
  inherit (lib) concatMap foldl' optionalAttrs recursiveUpdate;

  # The whole-configuration schema names the envelope annotations, every
  # component section, and `HermodTracing`, all as top-level properties.  Split
  # on whether a matching `schemas/<name>.schema.json` exists to tell the
  # sections from the rest, so an added or renamed section is picked up by a pin
  # bump rather than silently ignored.
  topLevelProperties =
    filter (k: k != "$schema")
      (attrNames (fromJSON (readFile "${cardanoConfigSrc}/schemas/config.schema.json")).properties);

  hasSectionSchema = name: pathExists "${cardanoConfigSrc}/schemas/${name}.schema.json";

  sections = filter hasSectionSchema topLevelProperties;

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

  # Envelope annotations, lifted out of the config body.  These are the
  # whole-config schema's top-level properties that are not a component section
  # and not `HermodTracing`, whose shape cardano-config leaves to
  # trace-dispatcher.  `$schema` is filtered out above, so add it back.
  envelopeKeys =
    ["$schema"]
    ++ filter (k: !(hasSectionSchema k) && k != "HermodTracing") topLevelProperties;

  # The format version this envelope declares, `currentFormatVersion` in
  # Schema.hs.  The schema only constrains it to `minimum: 1`, so read it from
  # the source; `mkConfigDrift` in default.nix fails the build if this and the
  # pin disagree.
  formatVersion = 1;

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
      configuration = foldl' (acc: key: recursiveUpdate acc (placeKey body key)) {} (attrNames body);
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
  # from a flat source.  Writing the configs in envelope shape deletes the
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

    # One quoted string per line, for a `[Text]` literal.
    strings = text: name:
      concatMap
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
    formatVersion = intDef schemaHs "currentFormatVersion";
  };

  # What the checker compares: our value against the pin's, per name.
  driftPairs = {
    inherit removedKeys tracingKeys tracingObsoleteKeys renamedKeys
      acceptedConnectionsLimitKeys formatVersion;
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
    inherit driftPairs mkEnvelope propertyToSection recognisedKeys removedKeys
      renamedKeys sections tracingKeys tracingObsoleteKeys unrecognisedKeys
      upstream;
  }
