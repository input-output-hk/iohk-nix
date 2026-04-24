##########################################################
###############            Leios           ###############
############### Cardano Node Configuration ###############
##########################################################
with builtins; {
  ##### Locations #####

  ByronGenesisFile = ./leios + "/byron-genesis.json";
  ByronGenesisHash = "c4f94b1f066f20547b8db2833e943cbe5d1bdc5c821eb491ea06ffe6a6e84573";
  ConwayGenesisFile = ./leios + "/conway-genesis.json";
  ConwayGenesisHash = "5060098993e3b1325a3c0732349380f03b5b659355fb8795112e07e7fc8542c4";
  ShelleyGenesisFile = ./leios + "/shelley-genesis.json";
  ShelleyGenesisHash = "3b7e31b71267c7e5cd2d23ddf4e5beb6b8e915dda8e5a404ba294955bbf03d08";
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
  # There's a syncing issue with GenesisMode being investigated
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
