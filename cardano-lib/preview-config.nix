##########################################################
###############          Preview           ###############
############### Cardano Node Configuration ###############
##########################################################
{
  ##### Locations #####
  ByronGenesisFile = ./preview + "/byron-genesis.json";
  ByronGenesisHash = "83de1d7302569ad56cf9139a41e2e11346d4cb4a31c00142557b6ab3fa550761";
  ShelleyGenesisFile = ./preview + "/shelley-genesis.json";
  ShelleyGenesisHash = "363498d1024f84bb39d3fa9593ce391483cb40d479b87233f868d6e57c3a400d";
  AlonzoGenesisFile = ./preview + "/alonzo-genesis.json";
  AlonzoGenesisHash = "7e94a15f55d1e82d10f09203fa1d40f8eede58fd8066542cf6566008068ed874";

  ### Core protocol parameters #####
  Protocol = "Cardano";
  RequiresNetworkMagic = "RequiresMagic";
  EnableP2P = true;
  TargetNumberOfActivePeers = 20;
  TargetNumberOfEstablishedPeers = 50;
  TargetNumberOfKnownPeers = 100;
  TargetNumberOfRootPeers = 100;
  TestEnableDevelopmentHardForkEras = true;
  TestEnableDevelopmentNetworkProtocols = true;
  TestShelleyHardForkAtEpoch = 0;
  TestAllegraHardForkAtEpoch = 0;
  TestAlonzoHardForkAtEpoch = 0;
  TestMaryHardForkAtEpoch = 0;

  ##### Update system Parameters #####

  LastKnownBlockVersion-Major = 3;
  LastKnownBlockVersion-Minor = 1;
  LastKnownBlockVersion-Alt = 0;

  ApplicationName = "cardano-sl";
  ApplicationVersion = 0;
}
