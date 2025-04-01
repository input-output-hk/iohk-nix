##########################################################
###############          Preview           ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./preview + "/byron-genesis.json";
  ByronGenesisHash = "83de1d7302569ad56cf9139a41e2e11346d4cb4a31c00142557b6ab3fa550761";
  ConwayGenesisFile = ./preview + "/conway-genesis.json";
  ConwayGenesisHash = "9cc5084f02e27210eacba47af0872e3dba8946ad9460b6072d793e1d2f3987ef";
  ShelleyGenesisFile = ./preview + "/shelley-genesis.json";
  ShelleyGenesisHash = "363498d1024f84bb39d3fa9593ce391483cb40d479b87233f868d6e57c3a400d";
  AlonzoGenesisFile = ./preview + "/alonzo-genesis.json";
  AlonzoGenesisHash = "7e94a15f55d1e82d10f09203fa1d40f8eede58fd8066542cf6566008068ed874";
  CheckPointsFile = "placeholder str: ./preview + ...";
  CheckPointsHash = "...";

  ### Core protocol parameters #####
  Protocol = "Cardano";

  RequiresNetworkMagic = "RequiresMagic";
  EnableP2P = true;
  PeerSharing = true;
  TargetNumberOfActivePeers = 20;
  TargetNumberOfEstablishedPeers = 40;
  TargetNumberOfKnownPeers = 150;
  TargetNumberOfRootPeers = 60;
  ExperimentalHardForksEnabled = false;
  ExperimentalProtocolsEnabled = false;
  TestShelleyHardForkAtEpoch = 0;
  TestAllegraHardForkAtEpoch = 0;
  TestAlonzoHardForkAtEpoch = 0;
  TestMaryHardForkAtEpoch = 0;

  # The consensus mode.  If set to "GenesisMode", the CheckPointsFile and
  # CheckpointsHash value above will be used and an absolute path to a peer
  # snapshot file will need to be declared in the p2p topology file under key
  # `peerSnapshotFile`.
  ConsensusMode = "PraosMode";

  # Default parameter values for "GenesisMode"
  SyncTargetNumberOfActivePeers = 0;
  SyncTargetNumberOfActiveBigLedgerPeers = 30;
  SyncTargetNumberOfEstablishedBigLedgerPeers = 50;
  SyncTargetNumberOfKnownBigLedgerPeers = 100;
  MinBigLedgerPeersForTrustedState = 5;

  ##### Update system parameters #####

  LastKnownBlockVersion-Major = 3;
  LastKnownBlockVersion-Minor = 1;
  LastKnownBlockVersion-Alt = 0;
}
