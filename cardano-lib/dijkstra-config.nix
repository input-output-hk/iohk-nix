##########################################################
###############          Dijkstra          ###############
############### Cardano Node Configuration ###############
##########################################################

with builtins; {
  ##### Locations #####

  ByronGenesisFile = ./dijkstra + "/byron-genesis.json";
  ByronGenesisHash = "f546656692a35f759d4d68365e7aa3dd7561e47a2523be720193eaa83c69f256";
  ConwayGenesisFile = ./dijkstra + "/conway-genesis.json";
  ConwayGenesisHash = "ace38afe42e7d21c381f8b4053bbc22ccaad40091be4a5ba428712081fb641f2";
  ShelleyGenesisFile = ./dijkstra + "/shelley-genesis.json";
  ShelleyGenesisHash = "ab181806a28d64e0f40db4e887c3f61910edc2a6733c534281ad4ba1d6b10349";
  AlonzoGenesisFile = ./dijkstra + "/alonzo-genesis.json";
  AlonzoGenesisHash = "cc143a4a73d7ab8cbc4bbd7750c91c5204cb597007cea3a317a1fc240c1954f1";
  DijkstraGenesisFile = ./dijkstra + "/dijkstra-genesis.json";
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
  # `CheckpointsFileHash` is not required for dijkstra.
  ConsensusMode = "PraosMode";

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
