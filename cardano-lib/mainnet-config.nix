##########################################################
###############          Mainnet           ###############
############### Cardano Node Configuration ###############
##########################################################

with builtins; {
  ##### Locations #####

  ByronGenesisFile = ./mainnet + "/byron-genesis.json";
  ByronGenesisHash = "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb";
  ConwayGenesisFile = ./mainnet + "/conway-genesis.json";
  ConwayGenesisHash = "15a199f895e461ec0ffc6dd4e4028af28a492ab4e806d39cb674c88f7643ef62";
  ShelleyGenesisFile = ./mainnet + "/shelley-genesis.json";
  ShelleyGenesisHash = "1a3be38bcbb7911969283716ad7aa550250226b76a61fc51cc9a9a35d9276d81";
  AlonzoGenesisFile = ./mainnet + "/alonzo-genesis.json";
  AlonzoGenesisHash = "7e94a15f55d1e82d10f09203fa1d40f8eede58fd8066542cf6566008068ed874";
  CheckpointsFile = ./mainnet + "/checkpoints.json";
  CheckpointsFileHash = "3e6dee5bae7acc6d870187e72674b37c929be8c66e62a552cf6a876b1af31ade";

  ##### Core protocol parameters #####

  # This is the instance of the Ouroboros family that we are running.
  # The node also supports various test and mock instances.
  # "RealPBFT" is the real (ie not mock) (permissive) OBFT protocol, which
  # is what we use on mainnet in Byron era.
  Protocol = "Cardano";

  # The mainnet does not include the network magic into addresses. Testnets do.
  RequiresNetworkMagic = "RequiresNoMagic";
  EnableP2P = true;
  PeerSharing = true;
  TargetNumberOfActivePeers = 20;
  TargetNumberOfEstablishedPeers = 30;
  TargetNumberOfKnownPeers = 150;
  TargetNumberOfRootPeers = 60;
  TraceMempool = false;

  MaxKnownMajorProtocolVersion = 2;

  # The consensus mode.  If set to "GenesisMode", the `CheckpointsFile` and
  # `CheckpointsFileHash` values above will be used and a path to a peer
  # snapshot file will need to be declared in the p2p topology file under key
  # `peerSnapshotFile`.
  ConsensusMode = "PraosMode";

  # Default "GenesisMode" parameter values
  SyncTargetNumberOfActivePeers = 0;
  SyncTargetNumberOfActiveBigLedgerPeers = 30;
  SyncTargetNumberOfEstablishedBigLedgerPeers = 40;
  SyncTargetNumberOfKnownBigLedgerPeers = 100;
  MinBigLedgerPeersForTrustedState = 5;

  # Default Ledger Configuration
  # Additional configuration options can be found at:
  # https://ouroboros-consensus.cardano.intersectmbo.org/docs/for-developers/utxo-hd/migrating
  LedgerDB = {
    # The time interval between snapshots, in seconds.
    SnapshotInterval = (fromJSON (readFile ./mainnet/shelley-genesis.json)).securityParam * 2;

    # The number of disk snapshots to keep.
    NumOfDiskSnapshots = 2;

    # When querying the store for a big range of UTxOs (such as with
    # QueryUTxOByAddress), the store will be read in batches of this size.
    QueryBatchSize = 100;

    # The backend can either be in memory with `V2InMemory` or on disk with
    # `V1LMDB`.
    Backend = "V2InMemory";
  };

  ##### Update system parameters #####

  # This protocol version number gets used by block producing nodes as part
  # part of the system for agreeing on and synchronising protocol updates.
  LastKnownBlockVersion-Major = 3;
  LastKnownBlockVersion-Minor = 0;
  LastKnownBlockVersion-Alt = 0;
}
