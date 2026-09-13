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
  inherit (builtins) attrNames elem filter fromJSON listToAttrs readFile;
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

  # Envelope annotations, lifted out of the config body.
  envelopeKeys = ["$schema" "Version" "MinNodeVersion" "Configuration"];

  # The format version this envelope declares, `currentFormatVersion` in
  # Schema.hs.  The schema gives no default to read it from.
  formatVersion = 1;

  # Where a flat key lands inside `Configuration`.  An unrecognised key is kept
  # at the top of `Configuration` rather than dropped, matching migrate, so
  # nothing is silently lost.  It still warns on the next parse.
  placeKey = body: key:
    let
      value = body.${key};
      nest = section: {${section} = {${key} = value;};};
    in
      if elem key removedKeys then {}
      else if elem key tracingKeys then nest "HermodTracing"
      else if propertyToSection ? ${key} then nest propertyToSection.${key}
      else {${key} = value;};

  mkEnvelope = nodeConfig:
    let
      body = removeAttrs nodeConfig envelopeKeys;
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
  unrecognisedKeys = allowed: nodeConfig:
    filter (k: !(elem k (recognisedKeys ++ allowed))) (attrNames nodeConfig);

in
  assert (builtins.length (attrNames propertyToSection)) == propertyCount;
  {
    inherit mkEnvelope propertyToSection recognisedKeys removedKeys tracingKeys
      unrecognisedKeys;
  }
