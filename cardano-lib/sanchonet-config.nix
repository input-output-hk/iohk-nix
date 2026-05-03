##########################################################
###############          Sanchonet         ###############
############### Cardano Node Configuration ###############
##########################################################

with builtins; {
  ##### Locations #####

  ByronGenesisFile = ./sanchonet + "/byron-genesis.json";
  ByronGenesisHash = "785eb88427e136378a15b0a152a8bfbeec7a611529ccda29c43a1e60ffb48eaa";
  ConwayGenesisFile = ./sanchonet + "/conway-genesis.json";
  ConwayGenesisHash = "ff31343b814adddfdcaceb76f635647e2b7d1adfc0ddac7c40b70cbdcd82fed9";
  ShelleyGenesisFile = ./sanchonet + "/shelley-genesis.json";
  ShelleyGenesisHash = "f94457ec45a0c6773057a529533cf7ccf746cb44dabd56ae970e1dbfb55bfdb2";
  AlonzoGenesisFile = ./sanchonet + "/alonzo-genesis.json";
  AlonzoGenesisHash = "8bedcaea62107d8a79ed5293b0027b3f8706a4bc2422f33380cb1fd01c6fa6ec";
  DijkstraGenesisFile = ./sanchonet + "/dijkstra-genesis.json";
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

  # The consensus mode.  If set to "GenesisMode", a path to a peer snapshot
  # file will need to be declared in the p2p topology file under key
  # `peerSnapshotFile`.  A `CheckpointsFile` and corresponding
  # `CheckpointsFileHash` is not required for sanchonet.
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
    SnapshotInterval = (fromJSON (readFile ./sanchonet/shelley-genesis.json)).securityParam * 2;

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
