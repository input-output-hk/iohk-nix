##########################################################
###############            Leios           ###############
############### Cardano Node Configuration ###############
##########################################################
with builtins; {
  ##### Locations #####

  ByronGenesisFile = ./leios + "/byron-genesis.json";
  ByronGenesisHash = "5809f031d8dc8ae8091f66a80ebe8f7d173e475436110c462a0533ed65954639";
  ConwayGenesisFile = ./leios + "/conway-genesis.json";
  ConwayGenesisHash = "e2951aa7f08dcd89bb6ca7fcf9acae5c46bdefb5a9affbac769bbe1902e982eb";
  ShelleyGenesisFile = ./leios + "/shelley-genesis.json";
  ShelleyGenesisHash = "1944510a4fd91415444285231058f6f6ff0f6f3ff3d0356c76c00c5a77f29567";
  AlonzoGenesisFile = ./leios + "/alonzo-genesis.json";
  AlonzoGenesisHash = "387a7c4880477ce7b128566fa7f9f9ed99ee04476084e9f6332b6d42d907faab";
  DijkstraGenesisFile = ./leios + "/dijkstra-genesis.json";
  DijkstraGenesisHash = "aa1238f505479a9b104d2cc001b4bf951062cd527200bea9a1857bdd0dc41085";

  ### Core protocol parameters #####
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

  # The consensus mode.  If set to "GenesisMode", a path to a peer snapshot
  # file will need to be declared in the p2p topology file under key
  # `peerSnapshotFile`.  A `CheckpointsFile` and corresponding
  # `CheckpointsFileHash` is not required for leios.
  # There's a syncing issue with GenesisMode being investigated.
  ConsensusMode = "PraosMode";

  # Leios specific customizations:
  MempoolCapacityBytesOverride = 2000000;

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

    # Instead of an object (attribute set) with individual options, a
    # predefined snapshot policy can be selected by name, e.g.
    # `Snapshots = "Mithril";`.
    Snapshots = {
      # The snapshot interval in slots.  Use `securityParam * 40` to provide
      # intra-epoch snapshot redundancy while minimizing potential IOWAIT stall on some
      # spec constrained machines during snapshot write.
      SnapshotInterval = (fromJSON (readFile ./leios/shelley-genesis.json)).securityParam * 40;

      # Slot offset at which snapshot scheduling begins.
      SlotOffset = 0;

      # A minimum duration between snapshots, in seconds (used to avoid excessive snapshots while syncing).
      # Default is 10 minutes.
      # RateLimit = 600;

      # Randomised snapshot delay range, in seconds.
      # Both Min and Max need to be specified, otherwise the default delay of (5min, 10min) will be used.
      # MinDelay = 300;
      # MaxDelay = 600;

      # The number of disk snapshots to keep.
      NumOfDiskSnapshots = 2;
    };
  };

  LeiosDbConfig = {
    # Can be "InMemory" or "SQLite", with the default being "SQLite".
    Backend = "SQLite";

    # If backend is "SQLite" an extra key is expected for file path. This can
    # be either an absolute path or a relative path to node --database-path
    # arg. The default is "leios.db".
    Filepath = "leios.db";
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
  };
}
