##########################################################
###############          Dijkstra          ###############
############### Cardano Node Configuration ###############
##########################################################
with builtins; {
  ##### Locations #####

  ByronGenesisFile = ./dijkstra + "/byron-genesis.json";
  ByronGenesisHash = "245e5b28a31c6bc3e8b78f1223294ae09e57b6d2543b51aba7cf9dadff161238";
  ConwayGenesisFile = ./dijkstra + "/conway-genesis.json";
  ConwayGenesisHash = "e18ab8efc363ad170142706723568f1520a57f0dc05354e1d458041da18a3704";
  ShelleyGenesisFile = ./dijkstra + "/shelley-genesis.json";
  ShelleyGenesisHash = "b47eb627b52bf9cfe5ffd865658b47b5747a088d150a9c27216de374e836c385";
  AlonzoGenesisFile = ./dijkstra + "/alonzo-genesis.json";
  AlonzoGenesisHash = "387a7c4880477ce7b128566fa7f9f9ed99ee04476084e9f6332b6d42d907faab";
  DijkstraGenesisFile = ./dijkstra + "/dijkstra-genesis.json";
  DijkstraGenesisHash = "ad58e282eff1f48c241faf6319f88ed54843821627a6be69470f05d7904e4601";

  ### Core protocol parameters #####
  Protocol = "Cardano";

  RequiresNetworkMagic = "RequiresMagic";

  # For node 11.1.3, set false until we switch to 11.2.0
  ExperimentalHardForksEnabled = false;

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
  # `CheckpointsFileHash` is not required for dijkstra.
  ConsensusMode = "GenesisMode";

  # Mempool timeout parameters must be either all set or all unset.
  # When unset cardano-node will use default values.
  # Default example values are:
  # MempoolTimeoutSoft = 1.0;
  # MempoolTimeoutHard = 1.5;
  # MempoolTimeoutCapacity = 5.0;

  # Peer Sharing Mode, Network Deadline and Sync Target Configuration
  # To avoid config drift these will use ouroboros-network defaults which may change over time.
  # The values shown below are examples.
  #
  # MinBigLedgerPeersForTrustedState = 5;
  # SyncTargetNumberOfActiveBigLedgerPeers = 30;
  # SyncTargetNumberOfActivePeers = 5;
  # SyncTargetNumberOfEstablishedBigLedgerPeers = 40;
  # SyncTargetNumberOfEstablishedPeers = 10;
  # SyncTargetNumberOfKnownBigLedgerPeers = 100;
  # SyncTargetNumberOfKnownPeers = 150;
  # SyncTargetNumberOfRootPeers = 0;
  # TargetNumberOfActiveBigLedgerPeers = 5;
  # TargetNumberOfActivePeers = 20;
  # TargetNumberOfEstablishedBigLedgerPeers = 10;
  # TargetNumberOfEstablishedPeers = 30;
  # TargetNumberOfKnownBigLedgerPeers = 15;
  #
  # Additionally, as of ouroboros-network `0.22.2` with cardano-node `10.6.0`,
  # the following three node config parameters which previously required
  # explicit configuration depending on whether the node is a forger are now
  # handled automatically.
  #
  # PeerSharing: false for forgers, true for non-forgers
  # TargetNumberOfKnownPeers: set according to forging status
  # TargetNumberOfRootPeers: set according to forging status

  # Active network override (dijkstra only)
  #
  # While the network is comprised only of IO Labs pools, the big ledger peer
  # set is too small to reach trusted state at the ouroboros-network default
  # of 5, which stalls a Genesis Mode sync.  Lower the threshold so a sync can
  # complete.
  MinBigLedgerPeersForTrustedState = 3;

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
      SnapshotInterval = (fromJSON (readFile ./dijkstra/shelley-genesis.json)).securityParam * 40;

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

  ##### Update system parameters #####

  LastKnownBlockVersion-Major = 3;
  LastKnownBlockVersion-Minor = 1;
  LastKnownBlockVersion-Alt = 0;
}
