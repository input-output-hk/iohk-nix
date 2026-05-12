##########################################################
###############          Preview           ###############
############### Cardano Node Configuration ###############
##########################################################

with builtins; {
  ##### Locations #####

  ByronGenesisFile = ./preview + "/byron-genesis.json";
  ByronGenesisHash = "83de1d7302569ad56cf9139a41e2e11346d4cb4a31c00142557b6ab3fa550761";
  ConwayGenesisFile = ./preview + "/conway-genesis.json";
  ConwayGenesisHash = "9cc5084f02e27210eacba47af0872e3dba8946ad9460b6072d793e1d2f3987ef";
  ShelleyGenesisFile = ./preview + "/shelley-genesis.json";
  ShelleyGenesisHash = "363498d1024f84bb39d3fa9593ce391483cb40d479b87233f868d6e57c3a400d";
  AlonzoGenesisFile = ./preview + "/alonzo-genesis.json";
  AlonzoGenesisHash = "7e94a15f55d1e82d10f09203fa1d40f8eede58fd8066542cf6566008068ed874";
  CheckpointsFile = ./preview + "/checkpoints.json";
  CheckpointsFileHash = "bb5056ff1ced9d68dd99720695789664f6bf6f0cb02a4010df09b813e225ac51";

  ### Core protocol parameters #####
  Protocol = "Cardano";

  RequiresNetworkMagic = "RequiresMagic";

  ExperimentalHardForksEnabled = false;
  ExperimentalProtocolsEnabled = false;
  TestShelleyHardForkAtEpoch = 0;
  TestAllegraHardForkAtEpoch = 0;
  TestAlonzoHardForkAtEpoch = 0;
  TestMaryHardForkAtEpoch = 0;

  # The consensus mode.  If set to "GenesisMode", the `CheckpointsFile` and
  # `CheckpointsFileHash` values above will be used and a path to a peer
  # snapshot file will need to be declared in the p2p topology file under key
  # `peerSnapshotFile`.
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
    # When querying the store for a big range of UTxOs (such as with
    # QueryUTxOByAddress), the store will be read in batches of this size.
    QueryBatchSize = 100000;

    # The backend can either be in memory with `V2InMemory` or on disk with
    # `V1LMDB`.
    Backend = "V2InMemory";

    Snapshots = {
      # TODO -- revise: The time interval between snapshots, in slots.
      SnapshotInterval = (fromJSON (readFile ./preview/shelley-genesis.json)).securityParam * 2;

      # A minimum duration between snapshots, in seconds (used to avoid excessive snapshots while syncing).
      # Default is 10 minutes.
      RateLimit = 600;

      # Randomised snapshot delay range, in seconds.
      # Both Min and Max need to be specified, otherwise the default delay of (5min, 10min) will be used
      MinDelay = 300;
      MaxDelay = 600;

      # The number of disk snapshots to keep.
      NumOfDiskSnapshots = 2;
    };
  };

  ##### Update system parameters #####

  LastKnownBlockVersion-Major = 3;
  LastKnownBlockVersion-Minor = 1;
  LastKnownBlockVersion-Alt = 0;
}
