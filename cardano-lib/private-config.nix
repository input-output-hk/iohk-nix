##########################################################
###############          Private           ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./private + "/byron-genesis.json";
  ByronGenesisHash = "fd639df89b39fcbcf82b98ccb48893ffabd541aae86282879466e5971c42a1f8";
  ShelleyGenesisFile = ./private + "/shelley-genesis.json";
  ShelleyGenesisHash = "813d248c2570aaec4336ca04f592c8a1b65896dff4b59143bc7fbb614204b7a0";
  AlonzoGenesisFile = ./private + "/alonzo-genesis.json";
  AlonzoGenesisHash = "1182ef7bc09a976e3a4085ea9fdbb48f3a60e3d80d8fbe1e3daad231e613e5b9";
  ConwayGenesisFile = ./private + "/conway-genesis.json";
  ConwayGenesisHash = "ba3a98545902c877b545c903b69318ae097ae4d1f3536882b051e1561556f23e";

  ### Core protocol parameters #####
  Protocol = "Cardano";
  RequiresNetworkMagic = "RequiresMagic";
  EnableP2P = true;
  PeerSharing = true;
  TargetNumberOfActivePeers = 20;
  TargetNumberOfEstablishedPeers = 50;
  TargetNumberOfKnownPeers = 150;
  TargetNumberOfRootPeers = 60;
  ExperimentalHardForksEnabled = true;
  ExperimentalProtocolsEnabled = true;
  TestShelleyHardForkAtEpoch = 0;
  TestAllegraHardForkAtEpoch = 0;
  TestAlonzoHardForkAtEpoch = 0;
  TestMaryHardForkAtEpoch = 0;
  TestBabbageHardForkAtEpoch = 0;

  ##### Update system Parameters #####
  LastKnownBlockVersion-Major = 3;
  LastKnownBlockVersion-Minor = 1;
  LastKnownBlockVersion-Alt = 0;
}
