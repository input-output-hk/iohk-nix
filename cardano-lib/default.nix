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

  defaultLogConfig = import ./generic-log-config.nix;
  defaultExplorerLogConfig = import ./explorer-log-config.nix;
  mkExplorerConfig = name: nodeConfig: lib.filterAttrs (k: v: v != null) {
    NetworkName = name;
    inherit (nodeConfig) RequiresNetworkMagic;
    NodeConfigFile = "${__toFile "config-${toString name}.json" (__toJSON nodeConfig)}";
  };
  defaultProxyLogConfig = import ./proxy-log-config.nix;

  mkProxyTopology = relay: writeText "proxy-topology-file" ''
    wallet:
      relays: [[{ host: ${relay} }]]
  '';
  environments = {
    mainnet = rec {
      useByronWallet = true;
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
      submitApiConfig = {
        GenesisHash = nodeConfig.ByronGenesisHash;
        inherit (networkConfig) RequiresNetworkMagic;
      } // defaultExplorerLogConfig;
      explorerConfig = mkExplorerConfig "mainnet" nodeConfig;
      usePeersFromLedgerAfterSlot = 29691317;
    };
    staging = rec {
      useByronWallet = true;
      relaysNew = "relays.staging.cardano.org";
      explorerUrl = "https://explorer.staging.cardano.org";
      smashUrl = "https://smash.staging.cardano.org";
      metadataUrl = "https://metadata.cardano-testnet.iohkdev.io";
      edgeNodes = [
        "3.125.10.61"
        "52.192.59.170"
        "18.136.145.112"
      ];
      edgePort = 3001;
      confKey = "mainnet_dryrun_full";
      private = false;
      networkConfig = import ./staging-config.nix;
      nodeConfig = networkConfig // defaultLogConfig;
      consensusProtocol = networkConfig.Protocol;
      submitApiConfig = {
        GenesisHash = nodeConfig.ByronGenesisHash;
        inherit (networkConfig) RequiresNetworkMagic;
      } // defaultExplorerLogConfig;
      explorerConfig = mkExplorerConfig "staging" nodeConfig;
      usePeersFromLedgerAfterSlot = 29444240;
    };
    testnet = rec {
      useByronWallet = true;
      relays = "relays.cardano-testnet.iohkdev.io";
      relaysNew = "relays-new.cardano-testnet.iohkdev.io";
      explorerUrl = "https://explorer.cardano-testnet.iohkdev.io";
      smashUrl = "https://smash.cardano-testnet.iohkdev.io";
      metadataUrl = "https://metadata.cardano-testnet.iohkdev.io";
      edgeNodes = [
        "3.125.94.58"
        "18.176.19.63"
        "13.251.186.36"
        "3.135.95.164"
      ];
      edgePort = 3001;
      confKey = "testnet_full";
      private = false;
      networkConfig = import ./testnet-config.nix;
      nodeConfig = networkConfig // defaultLogConfig;
      consensusProtocol = networkConfig.Protocol;
      submitApiConfig = {
        GenesisHash = nodeConfig.ByronGenesisHash;
        inherit (networkConfig) RequiresNetworkMagic;
      } // defaultExplorerLogConfig;
      explorerConfig = mkExplorerConfig "testnet" nodeConfig;
      usePeersFromLedgerAfterSlot = 26888469;
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
      usePeersFromLedgerAfterSlot = 320000;
    };
    alonzo-blue = rec {
      useByronWallet = false;
      private = false;
      relaysNew = "relays.alonzo-blue.dev.cardano.org";
      explorerUrl = "https://explorer.alonzo-blue.dev.cardano.org";
      smashUrl = "https://smash.alonzo-blue.dev.cardano.org";
      metadataUrl = "https://metadata.cardano-testnet.iohkdev.io";
      networkConfig = import ./alonzo-blue-config.nix;
      consensusProtocol = networkConfig.Protocol;
      nodeConfig = defaultLogConfig // networkConfig;
      edgePort = 3001;
      explorerConfig = mkExplorerConfig "alonzo-blue" nodeConfig;
    };
    alonzo-white = rec {
      useByronWallet = false;
      private = false;
      relaysNew = "relays.alonzo-white.dev.cardano.org";
      explorerUrl = "https://explorer.alonzo-white.dev.cardano.org";
      smashUrl = "https://smash.alonzo-white.dev.cardano.org";
      metadataUrl = "https://metadata.cardano-testnet.iohkdev.io";
      networkConfig = import ./alonzo-white-config.nix;
      consensusProtocol = networkConfig.Protocol;
      nodeConfig = defaultLogConfig // networkConfig;
      edgePort = 3001;
      explorerConfig = mkExplorerConfig "alonzo-white" nodeConfig;
    };
    alonzo-purple = rec {
      useByronWallet = false;
      private = false;
      relaysNew = "relays.alonzo-purple.dev.cardano.org";
      explorerUrl = "https://explorer.alonzo-purple.dev.cardano.org";
      smashUrl = "https://smash.alonzo-purple.dev.cardano.org";
      metadataUrl = "https://metadata.cardano-testnet.iohkdev.io";
      networkConfig = import ./alonzo-purple-config.nix;
      consensusProtocol = networkConfig.Protocol;
      nodeConfig = defaultLogConfig // networkConfig;
      edgePort = 3001;
      explorerConfig = mkExplorerConfig "alonzo-purple" nodeConfig;
    };
    alonzo-qa = rec {
      useByronWallet = false;
      private = false;
      relaysNew = "relays.alonzo-qa.dev.cardano.org";
      explorerUrl = "https://explorer.alonzo-qa.dev.cardano.org";
      smashUrl = "https://smash.alonzo-qa.dev.cardano.org";
      metadataUrl = "https://metadata.cardano-testnet.iohkdev.io";
      networkConfig = import ./alonzo-qa-config.nix;
      consensusProtocol = networkConfig.Protocol;
      nodeConfig = defaultLogConfig // networkConfig;
      edgePort = 3001;
      explorerConfig = mkExplorerConfig "alonzo-qa" nodeConfig;
    };
    # used for daedalus/cardano-wallet for local development
    shelley_qa = rec {
      useByronWallet = false;
      private = false;
      relaysNew = "relays-new.shelley-qa.dev.cardano.org";
      explorerUrl = "https://explorer.shelley-qa.dev.cardano.org";
      smashUrl = "https://smash.shelley-qa.dev.cardano.org";
      metadataUrl = "https://metadata.cardano-testnet.iohkdev.io";
      networkConfig = import ./shelley_qa-config.nix;
      consensusProtocol = networkConfig.Protocol;
      nodeConfig = defaultLogConfig // networkConfig;
      edgePort = 3001;
      explorerConfig = mkExplorerConfig "shelley_qa" nodeConfig;
      usePeersFromLedgerAfterSlot = 23574838;
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
    Cardano  = { n = "byron"; shelley = "shelley"; alonzo = "alonzo"; };
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
                          ${if p == "Cardano" then ''
                            <a class="button is-info" href="${env}-${protNames.${p}.shelley}-genesis.json">${protNames.${p}.shelley}Genesis</a>
                            <a class="button is-info" href="${env}-${protNames.${p}.alonzo}-genesis.json">${protNames.${p}.alonzo}Genesis</a>
                          '' else ""}
                          <a class="button is-info" href="${env}-topology.json">topology</a>
                          <a class="button is-primary" href="${env}-db-sync-config.json">db-sync config</a>
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
          ${jq}/bin/jq . < ${mkEdgeTopology { edgeNodes = [ value.relaysNew ]; valency = 2; }} > $out/${env}-topology.json
          ${jq}/bin/jq . < ${__toFile "${env}-db-sync-config.json" (__toJSON (value.explorerConfig // defaultExplorerLogConfig))} > $out/${env}-db-sync-config.json
        ''
      ) environments )
    }
    ${jq}/bin/jq . < ${__toFile "rest-config.json" (__toJSON defaultExplorerLogConfig)} > $out/rest-config.json
    echo "report cardano $out index.html" > $out/nix-support/hydra-build-products
  '';

in {
  inherit environments forEnvironments forEnvironmentsCustom eachEnv mkEdgeTopology mkProxyTopology cardanoConfig defaultLogConfig defaultExplorerLogConfig defaultProxyLogConfig mkConfigHtml mkExplorerConfig;
}
