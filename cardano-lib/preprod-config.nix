##########################################################
###############          Preprod           ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./preprod + "/byron-genesis.json";
  ByronGenesisHash = "d4b8de7a11d929a323373cbab6c1a9bdc931beffff11db111cf9d57356ee1937";
  ConwayGenesisFile = ./preprod + "/conway-genesis.json";
  ConwayGenesisHash = "0eb6adaec3fcb1fe286c1b4ae0da2a117eafc3add51e17577d36dd39eddfc3db";
  ShelleyGenesisFile = ./preprod + "/shelley-genesis.json";
  ShelleyGenesisHash = "162d29c4e1cf6b8a84f2d692e67a3ac6bc7851bc3e6e4afe64d15778bed8bd86";
  AlonzoGenesisFile = ./preprod + "/alonzo-genesis.json";
  AlonzoGenesisHash = "7e94a15f55d1e82d10f09203fa1d40f8eede58fd8066542cf6566008068ed874";

  ##### Core protocol parameters #####

  # This is the instance of the Ouroboros family that we are running.
  # The node also supports various test and mock instances.
  # "RealPBFT" is the real (ie not mock) (permissive) OBFT protocol, which
  # is what we use on mainnet in Byron era.
  Protocol = "Cardano";

  # The mainnet does not include the network magic into addresses. Testnets do.
  RequiresNetworkMagic = "RequiresMagic";
  EnableP2P = true;
  PeerSharing = true;
  TargetNumberOfActivePeers = 20;
  TargetNumberOfEstablishedPeers = 40;
  TargetNumberOfKnownPeers = 150;
  TargetNumberOfRootPeers = 60;

  # The consensus mode.  If set to "GenesisMode", a path to a peer snapshot
  # file will need to be declared in the p2p topology file under key
  # `peerSnapshotFile`.  A `CheckpointsFile` and corresponding
  # `CheckpointsFileHash` is not required for preprod.
  ConsensusMode = "PraosMode";

  # Default parameter values for "GenesisMode"
  SyncTargetNumberOfActivePeers = 0;
  SyncTargetNumberOfActiveBigLedgerPeers = 30;
  SyncTargetNumberOfEstablishedBigLedgerPeers = 50;
  SyncTargetNumberOfKnownBigLedgerPeers = 100;
  MinBigLedgerPeersForTrustedState = 5;

  ##### Update system parameters #####

  # This protocol version number gets used by block producing nodes as part
  # part of the system for agreeing on and synchronising protocol updates.
  LastKnownBlockVersion-Major = 2;
  LastKnownBlockVersion-Minor = 0;
  LastKnownBlockVersion-Alt = 0;
}
