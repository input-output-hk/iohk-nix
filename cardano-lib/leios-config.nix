##########################################################
###############            Leios           ###############
############### Cardano Node Configuration ###############
##########################################################
with builtins; {
  ##### Locations #####

  ByronGenesisFile = ./leios + "/byron-genesis.json";
  ByronGenesisHash = "73522baa9b5c2afecb8794a17a5fc0650ba6621c0d96d0a18d514d94d3755efe";
  ConwayGenesisFile = ./leios + "/conway-genesis.json";
  ConwayGenesisHash = "7b465adc9d084af42e3f7685e561e974e0804b7efeaed12cf2f1ee409e91cc99";
  ShelleyGenesisFile = ./leios + "/shelley-genesis.json";
  ShelleyGenesisHash = "35381da1578c03d357599dce448cc6f3dddd5ae1020145b7c1cbd607607500e0";
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

  # Leios specific customizations:
  MempoolCapacityBytesOverride = 5000000;

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
