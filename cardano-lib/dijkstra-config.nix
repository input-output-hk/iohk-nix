##########################################################
###############          Dijkstra          ###############
############### Cardano Node Configuration ###############
##########################################################

with builtins; {
  ##### Locations #####

  ByronGenesisFile = ./dijkstra + "/byron-genesis.json";
  ByronGenesisHash = "9bdbd85630be6848fc31425efe0be0f1f0bce8ee2def551f62e5a7bb165f552c";
  ConwayGenesisFile = ./dijkstra + "/conway-genesis.json";
  ConwayGenesisHash = "1882545753507dc03566b98b4c67752980797cd7e196323eef9baa0d474cbbb6";
  ShelleyGenesisFile = ./dijkstra + "/shelley-genesis.json";
  ShelleyGenesisHash = "a3d60a493cb73ab915084320cfaae32f9674c1394f52f98a261f855d42bdd885";
  AlonzoGenesisFile = ./dijkstra + "/alonzo-genesis.json";
  AlonzoGenesisHash = "cc143a4a73d7ab8cbc4bbd7750c91c5204cb597007cea3a317a1fc240c1954f1";
  DijkstraGenesisFile = ./dijkstra + "/dijkstra-genesis.json";
  DijkstraGenesisHash = "c6afe2641a407ee17f3625ed304571c3762e3123d38f4d8ae663f765f6c2bcb6";

  ### Core protocol parameters #####
  Protocol = "Cardano";

  RequiresNetworkMagic = "RequiresMagic";

  # For node 11.0.0, set false until the network is forked to PV11
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
  # See: https://developers.cardano.org/docs/get-started/cardano-node/p2p
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

  # Default Ledger Configuration
  # Additional configuration options can be found at:
  # https://ouroboros-consensus.cardano.intersectmbo.org/docs/for-developers/utxo-hd/migrating
  LedgerDB = {
    # The time interval between snapshots, in seconds.
    SnapshotInterval = (fromJSON (readFile ./dijkstra/shelley-genesis.json)).securityParam * 2;

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
