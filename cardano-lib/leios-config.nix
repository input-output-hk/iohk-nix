##########################################################
###############            Leios           ###############
############### Cardano Node Configuration ###############
##########################################################
with builtins; {
  ##### Locations #####

  ByronGenesisFile = ./leios + "/byron-genesis.json";
  ByronGenesisHash = "ddca2eff1541dea157e8c0cee0c04a7759e22f60d86277893278fbf6749a80f7";
  ConwayGenesisFile = ./leios + "/conway-genesis.json";
  ConwayGenesisHash = "a0cd1c00f7b6e6267544200ae84d56a800ae9ccb04d10da33e138760170f1e7b";
  ShelleyGenesisFile = ./leios + "/shelley-genesis.json";
  ShelleyGenesisHash = "6fb68ffebdd43232618801f6e88d37fbb01a84b08b92e50ffe6941c484820271";
  AlonzoGenesisFile = ./leios + "/alonzo-genesis.json";
  AlonzoGenesisHash = "387a7c4880477ce7b128566fa7f9f9ed99ee04476084e9f6332b6d42d907faab";
  DijkstraGenesisFile = ./leios + "/dijkstra-genesis.json";
  DijkstraGenesisHash = "c6afe2641a407ee17f3625ed304571c3762e3123d38f4d8ae663f765f6c2bcb6";

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

  # Leios specific customizations while it is on a ~10.5.1 versioning:
  # Node 10.5.x requires an explicitly declaration for P2P networking mode
  EnableP2P = true;
  # Node 10.5.x requires an explicit peer sharing value (this should be set false for bps)
  PeerSharing = true;
  # Node 10.5.x requires some explicit network params -- these should be 100, 100 for bps
  TargetNumberOfKnownPeers = 150;
  TargetNumberOfRootPeers = 60;
  # Leios specific adjustment
  MempoolCapacityBytesOverride = 25000000;

  # Default Ledger Configuration
  # Additional configuration options can be found at:
  # https://ouroboros-consensus.cardano.intersectmbo.org/docs/for-developers/utxo-hd/migrating
  LedgerDB = {
    # The time interval between snapshots, in seconds.
    SnapshotInterval = (fromJSON (readFile ./leios/shelley-genesis.json)).securityParam * 2;

    # The number of disk snapshots to keep.
    NumOfDiskSnapshots = 2;

    # When querying the store for a big range of UTxOs (such as with
    # QueryUTxOByAddress), the store will be read in batches of this size.
    QueryBatchSize = 100000;

    # The backend can either be in memory with `V2InMemory` or on disk with
    # `V1LMDB`.
    Backend = "V2InMemory";
  };

  ##### Update system parameters #####

  LastKnownBlockVersion-Major = 3;
  LastKnownBlockVersion-Minor = 1;
  LastKnownBlockVersion-Alt = 0;
}
