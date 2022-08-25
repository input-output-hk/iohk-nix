##########################################################
###############          Preview           ###############
############### Cardano Node Configuration ###############
##########################################################
{
  ##### Locations #####
  ByronGenesisFile = ./preview + "/byron-genesis.json";
  ByronGenesisHash = "72593f260b66f26bef4fc50b38a8f24d3d3633ad2e854eaf73039eb9402706f1";
  ShelleyGenesisFile = ./preview + "/shelley-genesis.json";
  ShelleyGenesisHash = "137fcc090936d258c06dedb5ad5b063c33ed5c4a71a0517b6466415ec83c7e14";
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
