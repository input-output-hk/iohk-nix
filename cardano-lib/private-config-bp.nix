##########################################################
#######                  Private                  ########
####### Cardano Node Block Producer Configuration ########
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./private + "/byron-genesis.json";
  ByronGenesisHash = "1c49093e43757a7f5d55791ee8682d2681e7a291b10dadc663ac1889b3f16984";
  ShelleyGenesisFile = ./private + "/shelley-genesis.json";
  ShelleyGenesisHash = "136177de30fc5d431cc80f5789089486c13629e589dd705aa4254f765a84582b";
  AlonzoGenesisFile = ./private + "/alonzo-genesis.json";
  AlonzoGenesisHash = "8bedcaea62107d8a79ed5293b0027b3f8706a4bc2422f33380cb1fd01c6fa6ec";
  ConwayGenesisFile = ./private + "/conway-genesis.json";
  ConwayGenesisHash = "de609b281cb3d8ae91a9d63a00c87092975612d603aa54c0f1c6a781e33d6e1e";

  ### Core protocol parameters #####
  Protocol = "Cardano";
  RequiresNetworkMagic = "RequiresMagic";
  EnableP2P = true;
  PeerSharing = false;
  TargetNumberOfActivePeers = 20;
  TargetNumberOfEstablishedPeers = 50;
  TargetNumberOfKnownPeers = 100;
  TargetNumberOfRootPeers = 100;
  ExperimentalHardForksEnabled = true;
  ExperimentalProtocolsEnabled = true;
  TestShelleyHardForkAtEpoch = 0;
  TestAllegraHardForkAtEpoch = 0;
  TestAlonzoHardForkAtEpoch = 0;
  TestMaryHardForkAtEpoch = 0;

  ##### Update system Parameters #####
  LastKnownBlockVersion-Major = 3;
  LastKnownBlockVersion-Minor = 1;
  LastKnownBlockVersion-Alt = 0;
}
