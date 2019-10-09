{lib, writeText}:
let
  mkEdgeTopology = {
    hostAddr ? "127.0.0.1"
  , port ? 3001
  , edgeHost ? "127.0.0.1"
  , edgeNodes ? []
  , edgePort ? if (edgeNodes != []) then 3001 else (if edgeHost == "127.0.0.1" then 7777 else 3001)
  , nodeId ? 0
  , valency ? 1
  }:
  let
    mkProducers = map (edgeHost': { addr = edgeHost'; port = edgePort; inherit valency; }) edgeNodes;
    topology = [
      {
        inherit nodeId;
        nodeAddress = {
          addr = hostAddr;
          inherit port;
        };
        producers = if (edgeNodes != []) then mkProducers else [
          {
            addr = edgeHost;
            port = edgePort;
            inherit valency;
          }
        ];
      }
    ];
  in builtins.toFile "topology.yaml" (builtins.toJSON topology);
  mkProxyTopology = relay: writeText "proxy-topology-file" ''
    wallet:
      relays: [[{ host: ${relay} }]]
  '';
  environments = {
    mainnet = {
      relays = "relays.cardano-mainnet.iohk.io";
      edgeNodes = [
        "18.185.45.45"
      ];
      edgePort = 3001;
      confKey = "mainnet_full";
      genesisFile = ./mainnet-genesis.json;
      genesisHash = "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb";
      private = false;
    };
    staging = {
      relays = "relays.awstest.iohkdev.io";
      edgeNodes = [
        "3.123.95.181"
      ];
      edgePort = 3001;
      confKey = "mainnet_dryrun_full";
      genesisFile = ./mainnet-genesis-dryrun-with-stakeholders.json;
      genesisHash = "c6a004d3d178f600cd8caa10abbebe1549bef878f0665aea2903472d5abf7323";
      private = false;
    };
    testnet = {
      relays = "relays.cardano-testnet.iohkdev.io";
      edgeNodes = [
        "18.194.162.74"
      ];
      edgePort = 3001;
      confKey = "testnet_full";
      genesisFile = ./testnet-genesis.json;
      genesisHash = "96fceff972c2c06bd3bb5243c39215333be6d56aaf4823073dca31afe5038471";
      private = false;
    };
    shelley_staging = {
      relays = "relays.shelley-staging.aws.iohkdev.io";
      edgeNodes = [
        "52.59.133.44"
        "3.114.127.167"
        "18.138.87.237"
      ];
      edgePort = 3001;
      confKey = "shelley_staging_full";
      genesisFile = ./shelley-staging-genesis.json;
      genesisHash = "82995abf3e0e0f8ab9a6448875536a1cba305f3ddde18cd5ff54c32d7a5978c6";
      private = false;
    };
    shelley_staging_short = {
      relays = "relays.staging-shelley-short.aws.iohkdev.io";
      edgeNodes = [
        "3.123.96.194"
      ];
      edgePort = 3001;
      confKey = "shelley_staging_short_full";
      genesisFile = ./shelley-staging-short-genesis.json;
      genesisHash = "a8e01a2325b31349e6f27d48c2e32e3a1ebaac2f8bc094114e780687f1400dda";
      pbftThreshold = "0.9";
      private = false;
    };
  };
  forEnvironments = f: lib.mapAttrs
    (name: env: f (env // { inherit name; }))
    environments;

  cardanoConfig = ./.;

in {
  inherit environments forEnvironments mkEdgeTopology mkProxyTopology cardanoConfig;
}
