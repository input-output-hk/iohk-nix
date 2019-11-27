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

  defaultLogConfig = import ./generic-log-config.nix;
  defaultExplorerLogConfig = import ./explorer-log-config.nix;

  mkProxyTopology = relay: writeText "proxy-topology-file" ''
    wallet:
      relays: [[{ host: ${relay} }]]
  '';
  environments = {
    mainnet = rec {
      relays = "relays.cardano-mainnet.iohk.io";
      edgeNodes = [
        "3.125.75.199"
        "18.177.103.105"
        "18.141.0.112"
        "52.14.58.121"
      ];
      edgePort = 3001;
      confKey = "mainnet_full";
      genesisFile = ./mainnet-genesis.json;
      genesisHash = "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb";
      private = false;
      networkConfig = import ./mainnet-config.nix;
      nodeConfig = networkConfig // defaultLogConfig;
    };
    staging = rec {
      relays = "relays.awstest.iohkdev.io";
      edgeNodes = [
        "3.125.10.61"
        "52.192.59.170"
        "18.136.145.112"
      ];
      edgePort = 3001;
      confKey = "mainnet_dryrun_full";
      genesisFile = ./mainnet-genesis-dryrun-with-stakeholders.json;
      genesisHash = "c6a004d3d178f600cd8caa10abbebe1549bef878f0665aea2903472d5abf7323";
      private = false;
      networkConfig = import ./staging-config.nix;
      nodeConfig = networkConfig // defaultLogConfig;
    };
    testnet = rec {
      relays = "relays.cardano-testnet.iohkdev.io";
      edgeNodes = [
        "3.125.94.58"
        "18.176.19.63"
        "13.251.186.36"
        "3.135.95.164"
      ];
      edgePort = 3001;
      confKey = "testnet_full";
      genesisFile = ./testnet-genesis.json;
      genesisHash = "96fceff972c2c06bd3bb5243c39215333be6d56aaf4823073dca31afe5038471";
      private = false;
      networkConfig = import ./testnet-config.nix;
      nodeConfig = networkConfig // defaultLogConfig;
    };
    shelley_staging = rec {
      relays = "relays.staging-shelley.aws.iohkdev.io";
      edgeNodes = [
        "3.125.23.159"
        "18.177.133.109"
        "18.141.119.164"
      ];
      edgePort = 3001;
      confKey = "shelley_staging_full";
      genesisFile = ./shelley-staging-genesis.json;
      genesisHash = "82995abf3e0e0f8ab9a6448875536a1cba305f3ddde18cd5ff54c32d7a5978c6";
      private = false;
      networkConfig = import ./shelley-staging-config.nix;
      nodeConfig = networkConfig // defaultLogConfig;
    };
    shelley_staging_short = rec {
      relays = "relays.staging-shelley-short.aws.iohkdev.io";
      edgeNodes = [
        "52.59.133.44"
        "3.114.127.167"
        "18.138.87.237"
      ];
      edgePort = 3001;
      confKey = "shelley_staging_short_full";
      genesisFile = ./shelley-staging-short-genesis.json;
      genesisHash = "78be790c7c4dec7bd2f690c40296e130fefdd198d1175f2b0e9d7e53675f8779";
      private = false;
      networkConfig = import ./shelley-staging-short-config.nix;
      nodeConfig = networkConfig // defaultLogConfig;
    };
    latency-tests = {
      relays = "relays.latency-tests.aws.iohkdev.io";
      edgeNodes = [
        "18.231.36.12"
      ];
      edgePort = 3001;
      confKey = "latency_tests_full";
      genesisFile = ./latency-tests-genesis.json;
      genesisHash = "c8b2ef02574d10bf23c2cd4a8c4022a9285f366af64b2544b317e2175b94f5a3";
      private = false;
    };
    mainnet-ci = {
      relays = "";
      edgeNodes = [
        "10.1.0.8"
      ];
      edgePort = 3000;
      confKey = "mainnet_ci_full";
      genesisFile = ./mainnet-ci-genesis.json;
      genesisHash = "12da51c484b5310fe26ca06ab24b94b323cde3698a0a50cb3f212abd08c2731e";
      private = false;
    };
  };
  forEnvironments = f: lib.mapAttrs
    (name: env: f (env // { inherit name; }))
    environments;
  forEnvironmentsCustom = f: environments: lib.mapAttrs
    (name: env: f (env // { inherit name; }))
    environments;

  cardanoConfig = ./.;

in {
  inherit environments forEnvironments forEnvironmentsCustom mkEdgeTopology mkProxyTopology cardanoConfig defaultLogConfig defaultExplorerLogConfig;
}
