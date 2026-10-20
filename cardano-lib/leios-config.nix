##########################################################
###############            Leios           ###############
############### Cardano Node Configuration ###############
##########################################################
{
  ##### Locations #####

  ByronGenesisFile = ./leios + "/byron-genesis.json";
  ByronGenesisHash = "3cbc289e3855d93a54877d5106743e1a77cdee3aaed675e62f19c2a373d698a2";
  ConwayGenesisFile = ./leios + "/conway-genesis.json";
  ConwayGenesisHash = "7e237f0c935539f5571297e3171946dd83929719496cbf51d0dab814a47ae0de";
  ShelleyGenesisFile = ./leios + "/shelley-genesis.json";
  ShelleyGenesisHash = "d1b19e006c1198fb7ac9d53073c1a9f77acb1a9474c92310006ae4418290cda3";
  AlonzoGenesisFile = ./leios + "/alonzo-genesis.json";
  AlonzoGenesisHash = "a0f11e3609a551b5f6cb8feeeea30cb5b70839ec9dffcd726a5323a1e3f3b12a";
  DijkstraGenesisFile = ./leios + "/dijkstra-genesis.json";
  DijkstraGenesisHash = "868a465b7115ae653ec532f46eca1a31a89211a96e810d78f82d6e60e3cdca87";

  ### Core protocol parameters #####

  # Dropped from the enveloped config, and nothing reads it from the legacy
  # config either.  Kept for now.  See mainnet-config.nix.
  Protocol = "Cardano";

  RequiresNetworkMagic = "RequiresMagic";

  ExperimentalHardForksEnabled = true;
  ExperimentalProtocolsEnabled = true;
  TestShelleyHardForkAtEpoch = 0;
  TestAllegraHardForkAtEpoch = 0;
  TestAlonzoHardForkAtEpoch = 0;
  TestMaryHardForkAtEpoch = 0;
  TestBabbageHardForkAtEpoch = 0;
  TestConwayHardForkAtEpoch = 0;
  TestDijkstraHardForkAtEpoch = 0;

  # The consensus mode.  If set to "GenesisMode", a path to a peer snapshot
  # file will need to be declared in the p2p topology file under key
  # `peerSnapshotFile`.  A `CheckpointsFile` and corresponding
  # `CheckpointsFileHash` is not required for leios.
  # There's a syncing issue with GenesisMode being investigated.
  ConsensusMode = "PraosMode";

  # Default Ledger Configuration
  # Additional configuration options can be found at:
  # https://ouroboros-consensus.cardano.intersectmbo.org/docs/for-developers/utxo-hd/migrating
  LedgerDB = {
    # When querying the store for a big range of UTxOs (such as with
    # QueryUTxOByAddress), the store will be read in batches of this size.
    QueryBatchSize = 100000;

    # The backend can either be in memory with `V2InMemory` or on disk with
    # `V2LSM`.
    Backend = "V2InMemory";

    # Derived from the security parameter, see snapshot-policy.nix.  Instead of
    # an object a predefined policy can be selected by name, e.g.
    # `Snapshots = "Mithril";`.
    #
    # A minimum duration between snapshots, in seconds, used to avoid excessive
    # snapshots while syncing.  Defaults to 10 minutes.
    # Snapshots.RateLimit = 600;
    Snapshots = import ./snapshot-policy.nix ./leios/shelley-genesis.json;
  };

  LeiosDbConfig = {
    # Can be "InMemory" or "SQLite", with the default being "SQLite".
    # As of w38a:
    #   leios.imm.db follows --database-path or --immutable-database-path
    #   leios.vol.db follows --database-path or --volatile-database-path
    Backend = "SQLite";
  };

  ##### Update system parameters #####

  LastKnownBlockVersion-Major = 3;
  LastKnownBlockVersion-Minor = 1;
  LastKnownBlockVersion-Alt = 0;

  # Leios tracer customizations.
  TraceOptions = {
    # Set the default logging to machine format.
    "" = {
      backends = [
        "EKGBackend"
        "Forwarder"
        "PrometheusSimple suffix 127.0.0.1 12798"
        "Stdout MachineFormat"
      ];
      detail = "DNormal";
      severity = "Notice";
    };

    "Consensus.LeiosKernel" = {
      severity = "Debug";
      maxFrequency = 0;
    };

    "Consensus.LeiosPeer" = {
      severity = "Debug";
      maxFrequency = 0;
    };

    "LeiosFetch.Remote" = {
      severity = "Debug";
      maxFrequency = 0;
    };

    "LeiosNotify.Remote" = {
      severity = "Debug";
      maxFrequency = 0;
    };

    "LeiosNotify.Remote.Receive.RequestNext" = {
      severity = "Silence";
    };

    "LeiosNotify.Remote.Receive.Votes" = {
      severity = "Silence";
    };

    "LeiosNotify.Remote.Send.RequestNext" = {
      severity = "Silence";
    };

    "LeiosNotify.Remote.Send.Votes" = {
      severity = "Silence";
    };
  };

  # Prefer Tx submission logic v2
  TxSubmissionLogicVersion = "TxSubmissionLogicV1";
}
