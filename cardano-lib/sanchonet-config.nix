##########################################################
###############         Sanchonet          ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./sanchonet + "/byron-genesis.json";
  ByronGenesisHash = "785eb88427e136378a15b0a152a8bfbeec7a611529ccda29c43a1e60ffb48eaa";
  ShelleyGenesisFile = ./sanchonet + "/shelley-genesis.json";
  ShelleyGenesisHash = "f94457ec45a0c6773057a529533cf7ccf746cb44dabd56ae970e1dbfb55bfdb2";
  AlonzoGenesisFile = ./sanchonet + "/alonzo-genesis.json";
  AlonzoGenesisHash = "8bedcaea62107d8a79ed5293b0027b3f8706a4bc2422f33380cb1fd01c6fa6ec";
  ConwayGenesisFile = ./sanchonet + "/conway-genesis.json";
  ConwayGenesisHash = "b9430bf2ed3ae29d7afa07fb08281e652550f613e282d06f32abfefce311f0de";

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
  TestAlonzoHardForkAtEpoch = 0;
  TestMaryHardForkAtEpoch = 0;

  ##### Update system Parameters #####

  LastKnownBlockVersion-Major = 3;
  LastKnownBlockVersion-Minor = 1;
  LastKnownBlockVersion-Alt = 0;
}
