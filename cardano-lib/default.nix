{lib, writeText, runCommand, jq}:
let
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
  in builtins.toFile "topology.yaml" (builtins.toJSON topology);

  mkEdgeTopologyP2P = {
    edgeNodes ? [{addr = "127.0.0.1"; port = 3001;}]
  , bootstrapPeers ? null
  , useLedgerAfterSlot ? 0
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
    };
  in
    builtins.toFile "topology.yaml" (builtins.toJSON topology);

  mkTopology = env: let
    legacyTopology = mkEdgeTopology {
      edgeNodes = [env.relaysNew];
      valency = 2;
      edgePort = env.edgePort or 3001;
    };
    p2pTopology = mkEdgeTopologyP2P {
      inherit (env) edgeNodes;

      # Until legacy mainnet relays are deprecated and replaced by IOG bootstrap peers for relaysNew,
      # filter the legacy relaysNew definition from the mainnet bootstrapPeers list.
      #
      # All other envs can use the edgeNodes list as bootstrapPeers.
      bootstrapPeers =
        if env.name == "mainnet"
        then
          map (e: {address = e.addr; inherit (e) port;})
            (builtins.filter (e: e.addr != env.relaysNew) env.edgeNodes)
        else
          map (e: {address = e.addr; inherit (e) port;}) env.edgeNodes;

      useLedgerAfterSlot = env.usePeersFromLedgerAfterSlot;
    };
  in
    if (env.nodeConfig.EnableP2P or false)
    then p2pTopology
    else legacyTopology;

  defaultLogConfig = import ./generic-log-config.nix;
  defaultExplorerLogConfig = import ./explorer-log-config.nix;

  mkExplorerConfig = name: nodeConfig: lib.filterAttrs (k: v: v != null) {
    NetworkName = name;
    inherit (nodeConfig) RequiresNetworkMagic;
    NodeConfigFile = "${__toFile "config-${toString name}.json" (__toJSON nodeConfig)}";
  };

  mkDbSyncConfig = name: nodeConfig: (mkExplorerConfig name nodeConfig) // defaultExplorerLogConfig;

  mkMithrilSignerConfig = name: env: {
    network = name;
    network_magic = (builtins.fromJSON (builtins.readFile env.networkConfig.ShelleyGenesisFile)).networkMagic;
    run_interval = 60000;
    store_retention_limit = 5;
  } // lib.optionalAttrs (env ? mithrilAggregatorEndpointUrl) {
    aggregator_endpoint = env.mithrilAggregatorEndpointUrl;
  } // lib.optionalAttrs (env ? mithrilEraReaderParams) {
    era_reader_adapter_type = "cardano-chain";
    era_reader_adapter_params = builtins.toJSON env.mithrilEraReaderParams;
  };

  mkSubmitApiConfig = name: nodeConfig: (lib.filterAttrs (k: v: v != null) {
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
  minNodeVersion = { MinNodeVersion = "8.12.0"; };

  environments = lib.mapAttrs (name: env: {
    inherit name;
    # default derived configs:
    nodeConfig = defaultLogConfig // env.networkConfig;
    nodeConfigBp = defaultLogConfig // env.networkConfigBp;
    consensusProtocol = env.networkConfig.Protocol;
    submitApiConfig = mkSubmitApiConfig name environments.${name}.nodeConfig;
    dbSyncConfig = mkDbSyncConfig name environments.${name}.nodeConfig;
    explorerConfig = mkExplorerConfig name environments.${name}.nodeConfig;
  } // env) {
    mainnet = rec {
      useByronWallet = true;
      private = false;
      domain = "cardano-mainnet.iohk.io";
      relays = "192.168.83.55";
      relaysNew = "192.168.83.55";
      explorerUrl = "https://explorer.cardano.org";
      smashUrl = "https://smash.cardano-mainnet.iohk.io";
      metadataUrl = "https://tokens.cardano.org";
      edgeNodes = [
        {
          addr = relaysNew;
          port = 3001;
        }
      ];
      edgePort = 3001;
      confKey = "mainnet_full";
      networkConfig = import ./mainnet-config.nix // minNodeVersion;
      networkConfigBp = import ./mainnet-config-bp.nix // minNodeVersion;
      usePeersFromLedgerAfterSlot = -1;
    };

    # Used for daedalus/cardano-wallet for local development
    shelley_qa = rec {
      useByronWallet = false;
      private = true;
      domain = "play.dev.cardano.org";
      relaysNew = "shelley-qa-node.play.dev.cardano.org";
      explorerUrl = "https://shelley-qa-explorer.play.dev.cardano.org";
      smashUrl = "https://shelley-qa-smash.play.dev.cardano.org";
      metadataUrl = "https://metadata.play.dev.cardano.org";
      edgeNodes = [
        {
          addr = relaysNew;
          port = 3001;
        }
      ];
      edgePort = 3001;
      networkConfig = import ./shelley_qa-config.nix // minNodeVersion;
      networkConfigBp = import ./shelley_qa-config-bp.nix // minNodeVersion;
      usePeersFromLedgerAfterSlot = 31348805;
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
      usePeersFromLedgerAfterSlot = 64454371;
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
      usePeersFromLedgerAfterSlot = 53827185;
    };

    sanchonet = rec {
      useByronWallet = false;
      private = false;
      domain = "play.dev.cardano.org";
      relaysNew = "sanchonet-node.play.dev.cardano.org";
      explorerUrl = "https://sanchonet-explorer.play.dev.cardano.org";
      smashUrl = "https://sanchonet-smash.play.dev.cardano.org";
      metadataUrl = "https://metadata.play.dev.cardano.org";
      mithrilAggregatorEndpointUrl = "https://aggregator.testing-sanchonet.api.mithril.network/aggregator";
      mithrilEraReaderParams = {
        address = "addr_test1qrg9v8xjjjjx95k2h2gquwrah8424798wqa5exuyhqpcggfyse0nuafkp7rnkxsssxue37259lfhemjdhs333u7v0gwsd0dr30";
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
      networkConfig = import ./sanchonet-config.nix // minNodeVersion;
      networkConfigBp = import ./sanchonet-config-bp.nix // minNodeVersion;
      usePeersFromLedgerAfterSlot = 33695977;
    };

    private = rec {
      useByronWallet = false;
      private = true;
      domain = "play.dev.cardano.org";
      relaysNew = "192.168.50.231";
      explorerUrl = "https://private-explorer.play.dev.cardano.org";
      smashUrl = "https://private-smash.play.dev.cardano.org";
      metadataUrl = "https://metadata.play.dev.cardano.org";
      edgeNodes = [
        {
          addr = relaysNew;
          port = 3001;
        }
      ];
      edgePort = 3001;
      networkConfig = import ./private-config.nix // minNodeVersion;
      networkConfigBp = import ./private-config-bp.nix // minNodeVersion;
      usePeersFromLedgerAfterSlot = 10000000000;
    };
  };

  # These will be removed at some point
  dead_environments = {
    # Network shutdown, but benchmarking configs reference it as a template
    testnet = __trace "DEPRECATION WARNING: TESTNET WAS SHUT DOWN. You may want to consider using preprod or preview." (rec {
      useByronWallet = true;
      private = true;
      relays = "doesnotexist.iog.io";
      relaysNew = "doesnotexist.iog.io";
      explorerUrl = "https://doesnotexist.iog.io";
      smashUrl = "https://doesnotexist.iog.io";
      metadataUrl = "https://doesnotexist.iog.io";
      edgeNodes = [];
      edgePort = 3001;
      confKey = "testnet_full";
      networkConfig = import ./testnet-config.nix // minNodeVersion;
      networkConfigBp = import ./testnet-config-bp.nix // minNodeVersion;
      consensusProtocol = networkConfig.Protocol;
      nodeConfig = defaultLogConfig // networkConfig;
      nodeConfigBp = defaultLogConfig // networkConfigBp;
      submitApiConfig = mkSubmitApiConfig "testnet" nodeConfig;
      dbSyncConfig = mkDbSyncConfig "testnet" nodeConfig;
      explorerConfig = mkExplorerConfig "testnet" nodeConfig;
      mithrilSignerConfig = mkMithrilSignerConfig "testnet" dead_environments.testnet;
      usePeersFromLedgerAfterSlot = -1;
    });
  };

  # TODO: add flag to disable with forEnvironments instead of hard-coded list?
  forEnvironments = f: lib.mapAttrs
    (name: env: f (env // { inherit name; }))
    environments;
  forEnvironmentsCustom = f: environments: lib.mapAttrs
    (name: env: f (env // { inherit name; }))
    environments;
  eachEnv = lib.flip lib.pipe [
    (lib.forEach (builtins.attrNames environments))
    lib.listToAttrs
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
                  ${toString (lib.mapAttrsToList (env: value:
                    let p = value.consensusProtocol;
                    in ''
                    <tr>
                      <td>${env}</td>
                      <td>
                        <div class="buttons has-addons">
                          <a class="button is-primary" href="${env}-config.json">config</a>
                          <a class="button is-primary" href="${env}-config-bp.json">block-producer config</a>
                          <a class="button is-info" href="${env}-${protNames.${p}.n}-genesis.json">${protNames.${p}.n}Genesis</a>
                          ${lib.optionalString (p == "Cardano") ''
                            <a class="button is-info" href="${env}-${protNames.${p}.shelley}-genesis.json">${protNames.${p}.shelley}Genesis</a>
                            <a class="button is-info" href="${env}-${protNames.${p}.alonzo}-genesis.json">${protNames.${p}.alonzo}Genesis</a>''}
                          ${lib.optionalString (p == "Cardano" && value.nodeConfig ? ConwayGenesisFile) ''
                            <a class="button is-info" href="${env}-${protNames.${p}.conway}-genesis.json">${protNames.${p}.conway}Genesis</a>''}
                          <a class="button is-info" href="${env}-topology.json">topology</a>
                          <a class="button is-primary" href="${env}-db-sync-config.json">db-sync config</a>
                          <a class="button is-primary" href="${env}-submit-api-config.json">submit-api config</a>
                          <a class="button is-primary" href="${env}-mithril-signer-config.json">mithril-signer config</a>
                          <a class="button is-primary" href="rest-config.json">rest config</a>
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
      toString (lib.mapAttrsToList (env: value:
        let
          p = value.consensusProtocol;
          genesisFile = { GenesisFile = "${env}-${protNames.${p}.n}-genesis.json"; };
          genesisFiles = {
            ByronGenesisFile = "${env}-${protNames.${p}.n}-genesis.json";
            ShelleyGenesisFile = "${env}-${protNames.${p}.shelley}-genesis.json";
            AlonzoGenesisFile = "${env}-${protNames.${p}.alonzo}-genesis.json";
          } // (lib.optionalAttrs (p == "Cardano" && value.nodeConfig ? ConwayGenesisFile) {
            ConwayGenesisFile = "${env}-${protNames.${p}.conway}-genesis.json";
          });
        in ''
          ${if p != "Cardano" then ''
            ${jq}/bin/jq . < ${__toFile "${env}-config.json" (__toJSON (value.nodeConfig // genesisFile))} > $out/${env}-config.json
            ${jq}/bin/jq . < ${__toFile "${env}-config-bp.json" (__toJSON (value.nodeConfigBp // genesisFile))} > $out/${env}-config-bp.json
          '' else ''
            ${jq}/bin/jq . < ${__toFile "${env}-config.json" (__toJSON (value.nodeConfig // genesisFiles))} > $out/${env}-config.json
            ${jq}/bin/jq . < ${__toFile "${env}-config-bp.json" (__toJSON (value.nodeConfigBp // genesisFiles))} > $out/${env}-config-bp.json
          ''}
          ${lib.optionalString (p == "RealPBFT" || p == "Byron") ''
            cp ${value.nodeConfig.GenesisFile} $out/${env}-${protNames.${p}.n}-genesis.json
          ''}
          ${lib.optionalString (p == "TPraos") ''
            cp ${value.nodeConfig.GenesisFile} $out/${env}-${protNames.${p}.n}-genesis.json
          ''}
          ${lib.optionalString (p == "Cardano") ''
            cp ${value.nodeConfig.ShelleyGenesisFile} $out/${env}-${protNames.${p}.shelley}-genesis.json
            cp ${value.nodeConfig.ByronGenesisFile} $out/${env}-${protNames.${p}.n}-genesis.json
            cp ${value.nodeConfig.AlonzoGenesisFile} $out/${env}-${protNames.${p}.alonzo}-genesis.json
          ''}
          ${lib.optionalString (p == "Cardano" && value.nodeConfig ? ConwayGenesisFile) ''
            cp ${value.nodeConfig.ConwayGenesisFile} $out/${env}-${protNames.${p}.conway}-genesis.json
          ''}
          ${jq}/bin/jq . < ${__toFile "${env}-db-sync-config.json" (__toJSON (value.dbSyncConfig // { NodeConfigFile = "${env}-config.json"; }))} > $out/${env}-db-sync-config.json
          ${jq}/bin/jq . < ${__toFile "${env}-submit-api-config.json" (__toJSON value.submitApiConfig)} > $out/${env}-submit-api-config.json
          ${jq}/bin/jq . < ${__toFile "${env}-mithril-signer-config.json" (__toJSON value.mithrilSignerConfig)} > $out/${env}-mithril-signer-config.json
          ${jq}/bin/jq . < ${mkTopology value} > $out/${env}-topology.json
        ''
      ) environments )
    }
    ${jq}/bin/jq . < ${__toFile "rest-config.json" (__toJSON defaultExplorerLogConfig)} > $out/rest-config.json
    echo "report cardano $out index.html" > $out/nix-support/hydra-build-products
  '';

in {
  inherit
    cardanoConfig
    defaultExplorerLogConfig
    defaultLogConfig
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

  # For now we export live and dead environemnts.
  environments = environments // dead_environments;
}
