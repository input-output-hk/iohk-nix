{lib, writeText, runCommand, jq }:
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

  defaultLogConfig = import ./generic-log-config.nix;
  defaultDbSyncLogConfig = import ./db-sync-log-config.nix;
  defaultExplorerLogConfig = import ./explorer-log-config.nix;

  mkExplorerConfig = name: nodeConfig: lib.filterAttrs (k: v: v != null) {
    NetworkName = name;
    inherit (nodeConfig) RequiresNetworkMagic;
    NodeConfigFile = "${__toFile "config-${toString name}.json" (__toJSON nodeConfig)}";
  };

  mkDbSyncConfig = name: nodeConfig:
    (lib.filterAttrs (k: v: v != null) {
      NetworkName = name;
      inherit (nodeConfig) RequiresNetworkMagic;
      NodeConfigFile = "${__toFile "config-${toString name}.json" (__toJSON nodeConfig)}";
    })
    // defaultDbSyncLogConfig;

  mkSubmitApiConfig = name: nodeConfig:
    (lib.filterAttrs (k: v: v != null) {
      GenesisHash = nodeConfig.ByronGenesisHash;
      inherit (nodeConfig) RequiresNetworkMagic;
    })
    // defaultDbSyncLogConfig;

  mkProxyTopology = relay: writeText "proxy-topology-file" ''
    wallet:
      relays: [[{ host: ${relay} }]]
  '';
  environments = {
    mainnet = rec {
      useByronWallet = true;
      domain = "cardano-mainnet.iohk.io";
      relays = "relays.cardano-mainnet.iohk.io";
      relaysNew = "relays-new.cardano-mainnet.iohk.io";
      explorerUrl = "https://explorer.cardano.org";
      smashUrl = "https://smash.cardano-mainnet.iohk.io";
      metadataUrl = "https://tokens.cardano.org";
      edgeNodes = [
        "3.125.75.199"
        "18.177.103.105"
        "18.141.0.112"
        "52.14.58.121"
      ];
      edgePort = 3001;
      confKey = "mainnet_full";
      private = false;
      networkConfig = import ./mainnet-config.nix;
      nodeConfig = networkConfig // defaultLogConfig;
      consensusProtocol = networkConfig.Protocol;
      submitApiConfig = mkSubmitApiConfig "mainnet" nodeConfig;
      dbSyncConfig = mkDbSyncConfig "mainnet" nodeConfig;
      explorerConfig = mkExplorerConfig "mainnet" nodeConfig;
      usePeersFromLedgerAfterSlot = 84916732;
      auxConfig = import ./aux-config/mainnet-aux.nix lib;
    };
    # Network shutdown, but benchmarking configs reference it as a template
    testnet = rec {
      useByronWallet = true;
      relays = "doesnotexist.iog.io";
      relaysNew = "doesnotexist.iog.io";
      explorerUrl = "https://doesnotexist.iog.io";
      smashUrl = "https://doesnotexist.iog.io";
      metadataUrl = "https://doesnotexist.iog.io";
      edgeNodes = [];
      edgePort = 3001;
      confKey = "testnet_full";
      private = true;
      networkConfig = import ./testnet-config.nix;
      nodeConfig = networkConfig // defaultLogConfig;
      consensusProtocol = networkConfig.Protocol;
      submitApiConfig = mkSubmitApiConfig "testnet" nodeConfig;
      explorerConfig = mkExplorerConfig "testnet" nodeConfig;
      usePeersFromLedgerAfterSlot = -1;
    };
    # used for daedalus/cardano-wallet for local development
    shelley_qa = rec {
      useByronWallet = false;
      private = true;
      domain = "shelley-qa.dev.cardano.org";
      relaysNew = "relays-new.shelley-qa.dev.cardano.org";
      relaysOld = "shelley-qa-node.world.dev.cardano.org";
      explorerUrl = "https://explorer.shelley-qa.dev.cardano.org";
      smashUrl = "https://smash.shelley-qa.dev.cardano.org";
      metadataUrl = "https://metadata.world.dev.cardano.org";
      networkConfig = import ./shelley_qa-config.nix;
      consensusProtocol = networkConfig.Protocol;
      nodeConfig = defaultLogConfig // networkConfig;
      edgePort = 3001;
      submitApiConfig = mkSubmitApiConfig "shelley_qa" nodeConfig;
      dbSyncConfig = mkDbSyncConfig "shelley_qa" nodeConfig;
      usePeersFromLedgerAfterSlot = 23574838;
    };

    p2p = rec {
      useByronWallet = false;
      private = false;
      relaysNew = "relays.p2p.dev.cardano.org";
      explorerUrl = "https://explorer.p2p.dev.cardano.org";
      smashUrl = "https://smash.p2p.dev.cardano.org";
      metadataUrl = "https://metadata.cardano-testnet.iohkdev.io";
      networkConfig = import ./p2p-config.nix;
      consensusProtocol = networkConfig.Protocol;
      nodeConfig = defaultLogConfig // networkConfig;
      edgePort = 3001;
      explorerConfig = mkExplorerConfig "p2p" nodeConfig;
      usePeersFromLedgerAfterSlot = 14680;
    };

    preprod = rec {
      useByronWallet = false;
      private = false;
      domain = "world.dev.cardano.org";
      relaysNew = "preprod-node.world.dev.cardano.org";
      explorerUrl = "https://preprod-explorer.world.dev.cardano.org";
      smashUrl = "https://preprod-smash.world.dev.cardano.org";
      metadataUrl = "https://metadata.world.dev.cardano.org";
      networkConfig = import ./preprod-config.nix;
      consensusProtocol = networkConfig.Protocol;
      nodeConfig = defaultLogConfig // networkConfig;
      edgeNodes = [
        {
          addr = relaysNew;
          port = 30000;
        }
      ];
      submitApiConfig = mkSubmitApiConfig "preprod" nodeConfig;
      dbSyncConfig = mkDbSyncConfig "preprod" nodeConfig;
      usePeersFromLedgerAfterSlot = 4642000;
    };
    preview = rec {
      useByronWallet = false;
      private = false;
      domain = "world.dev.cardano.org";
      relaysNew = "preview-node.world.dev.cardano.org";
      explorerUrl = "https://preview-explorer.world.dev.cardano.org";
      smashUrl = "https://preview-smash.world.dev.cardano.org";
      metadataUrl = "https://metadata.world.dev.cardano.org";
      networkConfig = import ./preview-config.nix;
      consensusProtocol = networkConfig.Protocol;
      nodeConfig = defaultLogConfig // networkConfig;
      edgeNodes = [
        {
          addr = relaysNew;
          port = 30002;
        }
      ];
      submitApiConfig = mkSubmitApiConfig "preview" nodeConfig;
      dbSyncConfig = mkDbSyncConfig "preview" nodeConfig;
      usePeersFromLedgerAfterSlot = 322000;
    };
    private = rec {
      useByronWallet = false;
      private = true;
      domain = "world.dev.cardano.org";
      relaysNew = "private-node.world.dev.cardano.org";
      explorerUrl = "https://private-explorer.world.dev.cardano.org";
      smashUrl = "https://private-smash.world.dev.cardano.org";
      metadataUrl = "https://metadata.world.dev.cardano.org";
      networkConfig = import ./private-config.nix;
      consensusProtocol = networkConfig.Protocol;
      nodeConfig = defaultLogConfig // networkConfig;
      edgeNodes = [
        {
          addr = relaysNew;
          port = 30007;
        }
      ];
      submitApiConfig = mkSubmitApiConfig "private" nodeConfig;
      dbSyncConfig = mkDbSyncConfig "private" nodeConfig;
      usePeersFromLedgerAfterSlot = 32000;
    };
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
                          <a class="button is-info" href="${env}-${protNames.${p}.n}-genesis.json">${protNames.${p}.n}Genesis</a>
                          ${lib.optionalString (p == "Cardano") ''
                            <a class="button is-info" href="${env}-${protNames.${p}.shelley}-genesis.json">${protNames.${p}.shelley}Genesis</a>
                            <a class="button is-info" href="${env}-${protNames.${p}.alonzo}-genesis.json">${protNames.${p}.alonzo}Genesis</a>
                          ''}
                          ${lib.optionalString (p == "Cardano" && value.nodeConfig ? ConwayGenesisFile) ''
                            <a class="button is-info" href="${env}-${protNames.${p}.conway}-genesis.json">${protNames.${p}.conway}Genesis</a>
                          ''}
                          <a class="button is-info" href="${env}-topology.json">topology</a>
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
        let p = value.consensusProtocol;
        in ''
          ${if p != "Cardano" then ''
            ${jq}/bin/jq . < ${__toFile "${env}-config.json" (__toJSON (value.nodeConfig // {
              GenesisFile = "${env}-${protNames.${p}.n}-genesis.json";
            }))} > $out/${env}-config.json
          '' else ''
            ${jq}/bin/jq . < ${__toFile "${env}-config.json" (__toJSON (value.nodeConfig // {
              ByronGenesisFile = "${env}-${protNames.${p}.n}-genesis.json";
              ShelleyGenesisFile = "${env}-${protNames.${p}.shelley}-genesis.json";
              AlonzoGenesisFile = "${env}-${protNames.${p}.alonzo}-genesis.json";
              ConwayGenesisFile = "${env}-${protNames.${p}.conway}-genesis.json";
            }))} > $out/${env}-config.json
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
          ${jq}/bin/jq . < ${mkEdgeTopology { edgeNodes = [ value.relaysNew ]; valency = 2; }} > $out/${env}-topology.json
        ''
      ) environments )
    }
    echo "report cardano $out index.html" > $out/nix-support/hydra-build-products
  '';

in {
  inherit environments forEnvironments forEnvironmentsCustom eachEnv mkEdgeTopology mkProxyTopology cardanoConfig defaultLogConfig defaultExplorerLogConfig mkConfigHtml mkExplorerConfig;
}
