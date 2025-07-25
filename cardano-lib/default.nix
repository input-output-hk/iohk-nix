{lib, writeText, runCommand, jq}:
let
  inherit (builtins) attrNames elem fromJSON getAttr readFile toFile toJSON;
  inherit (lib) filterAttrs flip forEach listToAttrs mapAttrs mapAttrsToList optionalAttrs optionalString pipe;

  mkEdgeTopology = {
    hostAddr ? "127.0.0.1"
  , port ? 3001
  , edgeHost ? "127.0.0.1"
  , edgeNodes ? []
  , edgePort ? if (edgeNodes != []) then 3001 else (if edgeHost == "127.0.0.1" then 7777 else 3001)
  , valency ? 1
  }:
  let
    mkProducers = map (edgeHost': { addr = edgeHost'; port = edgePort; inherit valency; }) edgeNodes;
    topology = {
      Producers = if (edgeNodes != []) then mkProducers else [
        {
          addr = edgeHost;
          port = edgePort;
          inherit valency;
        }
      ];
    };
  in toFile "topology.json" (toJSON topology);

  mkEdgeTopologyP2P = {
    edgeNodes ? [{addr = "127.0.0.1"; port = 3001;}]
  , bootstrapPeers ? null
  , useLedgerAfterSlot ? 0
  , peerSnapshotFile ? null
  }:
  let
    mkPublicRootsAccessPoints = map (edgeNode: {address = edgeNode.addr; port = edgeNode.port;}) edgeNodes;
    topology = {
      # If bootstrapPeers is null, publicRoots will consist of edgeNodes and useLedgerAfterSlot will be respected.
      # If bootstrapPeers is not null, any value of useLedgerAfter slot other than -1 will use ledger peers once
      # the chain is at the tip.
      inherit bootstrapPeers useLedgerAfterSlot;

      localRoots = [
        {
           accessPoints = [];
           advertise = false;
           valency = 1;
           trustable = false;
        }
      ];

      publicRoots = [
        {
          accessPoints = if bootstrapPeers == null then mkPublicRootsAccessPoints else [];
          advertise = false;
        }
      ];
    } // optionalAttrs (!(isNull peerSnapshotFile)) {
      inherit peerSnapshotFile;
    };
  in
    toFile "topology.json" (toJSON topology);

  mkTopology = env: let
    legacyTopology = mkEdgeTopology {
      edgeNodes = [env.relaysNew];
      valency = 2;
      edgePort = env.edgePort or 3001;
    };
    p2pTopology = mkEdgeTopologyP2P {
      inherit (env) edgeNodes;

      bootstrapPeers = map (e: {address = e.addr; inherit (e) port;}) env.edgeNodes;
      useLedgerAfterSlot = env.useLedgerAfterSlot;

      # Genesis mode is now default for preview and preprod as of node 10.5.0.
      #
      # As of node 10.5.0, the peer snapshot file can be added to the
      # topology file with a relative path to itself making the packaging
      # cleaner.
      #
      # As of node 10.6.0, the peer snapshot file can be added to the
      # topology file and not be fatal if missing while in PraosMode.
      #
      # Given that the peer snapshot file will be required as soon as genesis
      # mode is default, and migration to genesis mode for all networks is
      # coming soon, the snapshot will be declared in all network topologies
      # which also makes genesis testing a bit easier.
      peerSnapshotFile = "peer-snapshot.json";
    };
  in
    if (env.nodeConfig.EnableP2P or false)
    then p2pTopology
    else legacyTopology;

  defaultLogConfig = import ./generic-log-config.nix;
  defaultLogConfigLegacy = import ./generic-log-config-legacy.nix;
  defaultExplorerLogConfig = import ./explorer-log-config.nix;
  defaultTracerConfig = import ./generic-tracer-config.nix;

  mkExplorerConfig = name: nodeConfig: filterAttrs (k: v: v != null) {
    NetworkName = name;
    inherit (nodeConfig) RequiresNetworkMagic;
    NodeConfigFile = "${toFile "config-${toString name}.json" (toJSON nodeConfig)}";
  };

  mkDbSyncConfig = name: nodeConfig: dbSyncConfig:
    mkExplorerConfig name nodeConfig // defaultExplorerLogConfig // {
      # dbsync config not part of node config
      EnableFutureGenesis = dbSyncConfig.enableFutureGenesis or false;
    };

  mkMithrilSignerConfig = name: env: {
    network = name;
    network_magic = (fromJSON (readFile env.networkConfig.ShelleyGenesisFile)).networkMagic;
    run_interval = 60000;
    store_retention_limit = 5;
  } // optionalAttrs (env ? mithrilAggregatorEndpointUrl) {
    aggregator_endpoint = env.mithrilAggregatorEndpointUrl;
    era_reader_adapter_type = "cardano-chain";
    era_reader_adapter_params = toJSON env.mithrilEraReaderParams;
    genesis_verification_key = env.mithrilGenesisVerificationKey;
  };

  mkSubmitApiConfig = name: nodeConfig: (filterAttrs (k: v: v != null) {
    GenesisHash = nodeConfig.ByronGenesisHash;
    inherit (nodeConfig) RequiresNetworkMagic;
  })
  // defaultExplorerLogConfig;

  mkProxyTopology = relay: writeText "proxy-topology-file" ''
    wallet:
      relays: [[{ host: ${relay} }]]
  '';

  # Changes to minimum required node version typically occur with changes to
  # genesis files across all networks at once.  This defn will be applied to
  # all networks by default but can be overridden on a per network basis below
  # as needed.  Any node version string suffixes, such as `-pre`, should be
  # removed from this string identifier.
  #
  # Min currently 10.6.0 for proper default handling of PeerSharing,
  # TargetNumberOfKnownPeers and TargetNumberOfRootPeers parameters depending
  # on whether node is a forger or not.
  minNodeVersion = { MinNodeVersion = "10.6.0"; };

  mergeTraceOpts = cfg: traceOpts: cfg // {TraceOptions = getAttr "TraceOptions" cfg // traceOpts;};

  environments = mapAttrs (name: env: {
    inherit name;
    # default derived configs:
    nodeConfig = mergeTraceOpts (defaultLogConfig // env.networkConfig) (env.extraTracerConfig or {});
    nodeConfigLegacy = defaultLogConfigLegacy // env.networkConfig // (env.extraTracerConfigLegacy or {});
    nodeConfigBp = mergeTraceOpts (defaultLogConfig // env.networkConfigBp) (env.extraTracerConfig or {});
    nodeConfigBpLegacy = defaultLogConfigLegacy // env.networkConfigBp // (env.extraTracerConfigLegacy or {});
    tracerConfig = defaultTracerConfig // {inherit (fromJSON (readFile ./${name}/shelley-genesis.json)) networkMagic;};
    consensusProtocol = env.networkConfig.Protocol;
    submitApiConfig = mkSubmitApiConfig name environments.${name}.nodeConfig;
    dbSyncConfig =
      mkDbSyncConfig name environments.${name}.nodeConfig (env.extraDbSyncConfig or {});
    explorerConfig = mkExplorerConfig name environments.${name}.nodeConfig;
    mithrilSignerConfig = mkMithrilSignerConfig name env;
    peerSnapshot = fromJSON (readFile ./${name}/peer-snapshot.json);
  } // env) {
    mainnet = rec {
      useByronWallet = true;
      private = false;
      domain = "cardano-mainnet.iohk.io";
      relaysNew = "backbone.cardano.iog.io";
      explorerUrl = "https://explorer.cardano.org";
      smashUrl = "https://smash.cardano-mainnet.iohk.io";
      metadataUrl = "https://tokens.cardano.org";
      mithrilAggregatorEndpointUrl = "https://aggregator.release-mainnet.api.mithril.network/aggregator";
      mithrilAncillaryVerificationKey = "5b32332c37312c39362c3133332c34372c3235332c3232362c3133362c3233352c35372c3136342c3130362c3138362c322c32312c32392c3132302c3136332c38392c3132312c3137372c3133382c3230382c3133382c3231342c39392c35382c32322c302c35382c332c36395d";
      mithrilEraReaderParams = {
        address = "addr1qy72kwgm6kypyc5maw0h8mfagwag8wjnx6emgfnsnhqaml6gx7gg4tzplw9l32nsgclqax7stc4u6c5dn0ctljwscm2sqv0teg";
        verification_key = "5b31312c3133342c3231352c37362c3134312c3232302c3131312c3135342c36332c3233302c3131342c31322c38372c37342c39342c3137322c3133322c32372c39362c3138362c3132362c3137382c31392c3131342c33302c3234332c36342c3134312c3131302c38332c38362c31395d";
      };
      mithrilGenesisVerificationKey = "5b3139312c36362c3134302c3138352c3133382c31312c3233372c3230372c3235302c3134342c32372c322c3138382c33302c31322c38312c3135352c3230342c31302c3137392c37352c32332c3133382c3139362c3231372c352c31342c32302c35372c37392c33392c3137365d";
      edgeNodes = [
        {
          addr = relaysNew;
          port = 3001;
        }
        {
          addr = "backbone.mainnet.cardanofoundation.org";
          port = 3001;
        }
        {
          addr = "backbone.mainnet.emurgornd.com";
          port = 3001;
        }
      ];
      edgePort = 3001;
      confKey = "mainnet_full";
      networkConfig = import ./mainnet-config.nix // minNodeVersion;
      networkConfigBp = import ./mainnet-config-bp.nix // minNodeVersion;
      useLedgerAfterSlot = 157852837;
      extraDbSyncConfig = {
        enableFutureGenesis = true;
      };

      # Once legacy tracing system is removed, tracing mods can be placed back in $ENV-config.nix
      extraTracerConfig.Mempool.severity = "Silence";
      extraTracerConfigLegacy.TraceMempool = false;
    };

    preprod = rec {
      useByronWallet = false;
      private = false;
      domain = "play.dev.cardano.org";
      relaysNew = "preprod-node.play.dev.cardano.org";
      explorerUrl = "https://preprod-explorer.play.dev.cardano.org";
      smashUrl = "https://preprod-smash.play.dev.cardano.org";
      metadataUrl = "https://metadata.play.dev.cardano.org";
      mithrilAggregatorEndpointUrl = "https://aggregator.release-preprod.api.mithril.network/aggregator";
      mithrilAncillaryVerificationKey = "5b3138392c3139322c3231362c3135302c3131342c3231362c3233372c3231302c34352c31382c32312c3139362c3230382c3234362c3134362c322c3235322c3234332c3235312c3139372c32382c3135372c3230342c3134352c33302c31342c3232382c3136382c3132392c38332c3133362c33365d";
      mithrilEraReaderParams = {
        address = "addr_test1qpkyv2ws0deszm67t840sdnruqgr492n80g3y96xw3p2ksk6suj5musy6w8lsg3yjd09cnpgctc2qh386rtxphxt248qr0npnx";
        verification_key = "5b35352c3232382c3134342c38372c3133382c3133362c34382c382c31342c3138372c38352c3134382c39372c3233322c3235352c3232392c33382c3234342c3234372c3230342c3139382c31332c33312c3232322c32352c3136342c35322c3130322c39312c3132302c3230382c3134375d";
      };
      mithrilGenesisVerificationKey = "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d";
      edgeNodes = [
        {
          addr = relaysNew;
          port = 3001;
        }
      ];
      edgePort = 3001;
      networkConfig = import ./preprod-config.nix // minNodeVersion;
      networkConfigBp = import ./preprod-config-bp.nix // minNodeVersion;
      useLedgerAfterSlot = 93830456;
      extraDbSyncConfig = {
        enableFutureGenesis = true;
      };
    };

    preview = rec {
      useByronWallet = false;
      private = false;
      domain = "play.dev.cardano.org";
      relaysNew = "preview-node.play.dev.cardano.org";
      explorerUrl = "https://preview-explorer.play.dev.cardano.org";
      smashUrl = "https://preview-smash.play.dev.cardano.org";
      metadataUrl = "https://metadata.play.dev.cardano.org";
      mithrilAggregatorEndpointUrl = "https://aggregator.pre-release-preview.api.mithril.network/aggregator";
      mithrilAncillaryVerificationKey = "5b3138392c3139322c3231362c3135302c3131342c3231362c3233372c3231302c34352c31382c32312c3139362c3230382c3234362c3134362c322c3235322c3234332c3235312c3139372c32382c3135372c3230342c3134352c33302c31342c3232382c3136382c3132392c38332c3133362c33365d";
      mithrilEraReaderParams = {
        address = "addr_test1qrv5xfwh043mlc3vk5d97s4nmhxu7cmleyssvhx37gkfyejfe8d38v3vsfgetjafgrsdc49krug8wf04h5rmtengtejqlxrksk";
        verification_key = "5b35352c3232382c3134342c38372c3133382c3133362c34382c382c31342c3138372c38352c3134382c39372c3233322c3235352c3232392c33382c3234342c3234372c3230342c3139382c31332c33312c3232322c32352c3136342c35322c3130322c39312c3132302c3230382c3134375d";
      };
      mithrilGenesisVerificationKey = "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d";
      edgeNodes = [
        {
          addr = relaysNew;
          port = 3001;
        }
      ];
      edgePort = 3001;
      networkConfig = import ./preview-config.nix // minNodeVersion;
      networkConfigBp = import ./preview-config-bp.nix // minNodeVersion;
      useLedgerAfterSlot = 83116868;
      extraDbSyncConfig = {
        enableFutureGenesis = true;
      };
    };
  };

  # Move dead envs here for a grace period with an added deprecation warn trace prior to deletion.
  dead_environments = {
  };

  # TODO: add flag to disable with forEnvironments instead of hard-coded list?
  forEnvironments = f: mapAttrs
    (name: env: f (env // { inherit name; }))
    environments;
  forEnvironmentsCustom = f: environments: mapAttrs
    (name: env: f (env // { inherit name; }))
    environments;
  eachEnv = flip pipe [
    (forEach (attrNames environments))
    listToAttrs
  ];

  cardanoConfig = ./.;

  protNames = {
    RealPBFT = { n = "byron"; };
    TPraos   = { n = "shelley"; };
    Cardano  = { n = "byron"; shelley = "shelley"; alonzo = "alonzo"; conway = "conway"; };
  };

  configHtml = environments:
    ''
    <!DOCTYPE html>
    <html>
      <head>
        <title>Cardano Configurations</title>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bulma@0.8.0/css/bulma.min.css">
        <script defer src="https://use.fontawesome.com/releases/v5.3.1/js/all.js"></script>
      </head>
      <body>
        <section class="hero is-small is-primary">
          <div class="hero-body">
            <div class="container">
              <h1 class="title is-1">
                Cardano
              </h1>
              <h2 class="subtitle is-3">
                Configurations
              </h2>
            </div>
          </div>
        </section>

        <section class="section">
          <div class="container">
            <div class="table-container">
              <table class="table is-narrow is-fullwidth">
                <thead>
                  <tr>
                    <th>Cluster</th>
                    <th>Config</th>
                  </tr>
                </thead>
                <tbody>
                  ${toString (mapAttrsToList (env: value:
                    let p = value.consensusProtocol;
                    in ''
                    <tr>
                      <td>${env}</td>
                      <td>
                        <div class="buttons has-addons">
                          <a class="button is-primary" href="${env}-config.json">config</a>
                          <a class="button is-primary" href="${env}-config-bp.json">block-producer config</a>
                          <a class="button is-primary" href="${env}-config-legacy.json">config (legacy)</a>
                          <a class="button is-primary" href="${env}-config-bp-legacy.json">block-producer config (legacy)</a>
                          <a class="button is-info" href="${env}-${protNames.${p}.n}-genesis.json">${protNames.${p}.n}Genesis</a>
                          ${optionalString (p == "Cardano") ''
                            <a class="button is-info" href="${env}-${protNames.${p}.shelley}-genesis.json">${protNames.${p}.shelley}Genesis</a>
                            <a class="button is-info" href="${env}-${protNames.${p}.alonzo}-genesis.json">${protNames.${p}.alonzo}Genesis</a>''}
                          ${optionalString (p == "Cardano" && value.nodeConfig ? ConwayGenesisFile) ''
                            <a class="button is-info" href="${env}-${protNames.${p}.conway}-genesis.json">${protNames.${p}.conway}Genesis</a>''}
                          <a class="button is-info" href="${env}-topology.json">topology</a>
                          <a class="button is-info" href="${env}-peer-snapshot.json">peer-snapshot</a>
                          ${optionalString (value.nodeConfig ? CheckpointsFile) ''
                            <a class="button is-info" href="${env}-checkpoints.json">checkpoints</a>''}
                          <a class="button is-primary" href="${env}-db-sync-config.json">db-sync config</a>
                          <a class="button is-primary" href="${env}-submit-api-config.json">submit-api config</a>
                          <a class="button is-primary" href="${env}-mithril-signer-config.json">mithril-signer config</a>
                          <a class="button is-primary" href="rest-config.json">rest config</a>
                          <a class="button is-primary" href="${env}-tracer-config.json">tracer config</a>
                        </div>
                      </td>
                    </tr>
                    ''
                  ) environments) }
                </tbody>
              </table>
            </div>
          </div>
        </section>
      </body>
    </html>
  '';

  # Any environments using the HFC protocol of "Cardano" need a second genesis file attribute of
  # genesisFileHfc in order to generate the html table in mkConfigHtml
  mkConfigHtml = environments: runCommand "cardano-html" { buildInputs = [ jq ]; } ''
    mkdir -p $out/nix-support
    cp ${writeText "config.html" (configHtml environments)} $out/index.html
    ${
      toString (mapAttrsToList (env: value:
        let
          p = value.consensusProtocol;
          genesisFile = { GenesisFile = "${env}-${protNames.${p}.n}-genesis.json"; };
          genesisFiles = {
            ByronGenesisFile = "${env}-${protNames.${p}.n}-genesis.json";
            ShelleyGenesisFile = "${env}-${protNames.${p}.shelley}-genesis.json";
            AlonzoGenesisFile = "${env}-${protNames.${p}.alonzo}-genesis.json";
          } // (optionalAttrs (p == "Cardano" && value.nodeConfig ? ConwayGenesisFile) {
            ConwayGenesisFile = "${env}-${protNames.${p}.conway}-genesis.json";
          }) // (optionalAttrs (value.nodeConfig ? CheckpointsFile) {
            CheckpointsFile = "${env}-checkpoints.json";
          });
        in ''
          ${if p != "Cardano" then ''
            ${jq}/bin/jq . < ${toFile "${env}-config.json" (toJSON (value.nodeConfig // genesisFile))} > $out/${env}-config.json
            ${jq}/bin/jq . < ${toFile "${env}-config-bp.json" (toJSON (value.nodeConfigBp // genesisFile))} > $out/${env}-config-bp.json
            ${jq}/bin/jq . < ${toFile "${env}-config-legacy.json" (toJSON (value.nodeConfigLegacy // genesisFile))} > $out/${env}-config-legacy.json
            ${jq}/bin/jq . < ${toFile "${env}-config-bp-legacy.json" (toJSON (value.nodeConfigBpLegacy // genesisFile))} > $out/${env}-config-bp-legacy.json
          '' else ''
            ${jq}/bin/jq . < ${toFile "${env}-config.json" (toJSON (value.nodeConfig // genesisFiles))} > $out/${env}-config.json
            ${jq}/bin/jq . < ${toFile "${env}-config-bp.json" (toJSON (value.nodeConfigBp // genesisFiles))} > $out/${env}-config-bp.json
            ${jq}/bin/jq . < ${toFile "${env}-config-legacy.json" (toJSON (value.nodeConfigLegacy // genesisFiles))} > $out/${env}-config-legacy.json
            ${jq}/bin/jq . < ${toFile "${env}-config-bp-legacy.json" (toJSON (value.nodeConfigBpLegacy // genesisFiles))} > $out/${env}-config-bp-legacy.json
          ''}
          ${optionalString (p == "RealPBFT" || p == "Byron") ''
            cp ${value.nodeConfig.GenesisFile} $out/${env}-${protNames.${p}.n}-genesis.json
          ''}
          ${optionalString (p == "TPraos") ''
            cp ${value.nodeConfig.GenesisFile} $out/${env}-${protNames.${p}.n}-genesis.json
          ''}
          ${optionalString (p == "Cardano") ''
            cp ${value.nodeConfig.ShelleyGenesisFile} $out/${env}-${protNames.${p}.shelley}-genesis.json
            cp ${value.nodeConfig.ByronGenesisFile} $out/${env}-${protNames.${p}.n}-genesis.json
            cp ${value.nodeConfig.AlonzoGenesisFile} $out/${env}-${protNames.${p}.alonzo}-genesis.json
          ''}
          ${optionalString (p == "Cardano" && value.nodeConfigLegacy ? ConwayGenesisFile) ''
            cp ${value.nodeConfig.ConwayGenesisFile} $out/${env}-${protNames.${p}.conway}-genesis.json
          ''}
          ${jq}/bin/jq . < ${toFile "${env}-db-sync-config.json" (toJSON (value.dbSyncConfig // { NodeConfigFile = "${env}-config.json"; }))} > $out/${env}-db-sync-config.json
          ${jq}/bin/jq . < ${toFile "${env}-submit-api-config.json" (toJSON value.submitApiConfig)} > $out/${env}-submit-api-config.json
          ${jq}/bin/jq . < ${toFile "${env}-mithril-signer-config.json" (toJSON value.mithrilSignerConfig)} > $out/${env}-mithril-signer-config.json
          ${jq}/bin/jq . < ${toFile "${env}-tracer-config.json" (toJSON value.tracerConfig)} > $out/${env}-tracer-config.json
          ${jq}/bin/jq . < ${mkTopology value} > $out/${env}-topology.json
          ${jq}/bin/jq . < ${./${env}/peer-snapshot.json} > $out/${env}-peer-snapshot.json
          ${optionalString (value.nodeConfig ? CheckpointsFile) ''
            ${jq}/bin/jq . < ${./${env}/checkpoints.json} > $out/${env}-checkpoints.json
          ''}
        ''
      ) environments )
    }
    ${jq}/bin/jq . < ${toFile "rest-config.json" (toJSON defaultExplorerLogConfig)} > $out/rest-config.json
    echo "report cardano $out index.html" > $out/nix-support/hydra-build-products
  '';

in {
  inherit
    cardanoConfig
    defaultExplorerLogConfig
    defaultLogConfig
    defaultLogConfigLegacy
    defaultTracerConfig
    eachEnv
    forEnvironments
    forEnvironmentsCustom
    mkConfigHtml
    mkEdgeTopology
    mkEdgeTopologyP2P
    mkExplorerConfig
    mkMithrilSignerConfig
    mkProxyTopology
    mkTopology
    ;

  # For now we export live and dead environments.
  environments = environments // dead_environments;
}
