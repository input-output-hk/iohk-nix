##########################################################
###############          Private           ###############
############### Cardano Node Configuration ###############
##########################################################
{
  ##### Locations #####
  ByronGenesisFile = ./private + "/byron-genesis.json";
  ByronGenesisHash = "9a2d021950a717b48ee26079178ea0740e2b67ee9bcfcf4623f81f7c66d7c583";
  ShelleyGenesisFile = ./private + "/shelley-genesis.json";
  ShelleyGenesisHash = "d9d01796f5cfab8da3833dd98d15b3967fecb52ab75df9f0267be7888d535ead";
  AlonzoGenesisFile = ./private + "/alonzo-genesis.json";
  AlonzoGenesisHash = "7e94a15f55d1e82d10f09203fa1d40f8eede58fd8066542cf6566008068ed874";
  ConwayGenesisFile = ./private + "/conway-genesis.json";
  ConwayGenesisHash = "f28f1c1280ea0d32f8cd3143e268650d6c1a8e221522ce4a7d20d62fc09783e1";

  ### Core protocol parameters #####
  Protocol = "Cardano";
  RequiresNetworkMagic = "RequiresMagic";
  EnableP2P = true;
  TargetNumberOfActivePeers = 20;
  TargetNumberOfEstablishedPeers = 50;
  TargetNumberOfKnownPeers = 100;
  TargetNumberOfRootPeers = 100;
  ExperimentalHardForksEnabled = true;
  ExperimentalProtocolsEnabled = true;
  TestShelleyHardForkAtEpoch = 0;
  TestAllegraHardForkAtEpoch = 0;
  TestMaryHardForkAtEpoch = 0;
  TestAlonzoHardForkAtEpoch = 0;


  ##### Update system Parameters #####
  LastKnownBlockVersion-Major = 6;
  LastKnownBlockVersion-Minor = 0;
  LastKnownBlockVersion-Alt = 0;

  ApplicationName = "cardano-sl";
  ApplicationVersion = 0;
}
