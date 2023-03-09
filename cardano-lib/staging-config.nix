##########################################################
###############          Staging           ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./staging + "/byron-genesis.json";
  ByronGenesisHash = "c6a004d3d178f600cd8caa10abbebe1549bef878f0665aea2903472d5abf7323";
  ShelleyGenesisFile = ./staging + "/shelley-genesis.json";
  ShelleyGenesisHash = "cbb0f57a001120ef11d97b943ce6953b75d26adc11dc68451e9dec7989a62c85";
  AlonzoGenesisFile = ./staging + "/alonzo-genesis.json";
  AlonzoGenesisHash = "7e94a15f55d1e82d10f09203fa1d40f8eede58fd8066542cf6566008068ed874";
  ConwayGenesisFile = ./mainnet + "/conway-genesis.json";
  ConwayGenesisHash = "f28f1c1280ea0d32f8cd3143e268650d6c1a8e221522ce4a7d20d62fc09783e1";

  ##### Core protocol parameters #####

  # This is the instance of the Ouroboros family that we are running.
  # The node also supports various test and mock instances.
  # "RealPBFT" is the real (ie not mock) (permissive) OBFT protocol, which
  # is what we use on mainnet in Byron era.
  Protocol = "Cardano";

  # The mainnet does not include the network magic into addresses. Testnets do.
  RequiresNetworkMagic = "RequiresNoMagic";

  MaxKnownMajorProtocolVersion = 2;

  ##### Update system parameters #####

  # This protocol version number gets used by block producing nodes as part
  # part of the system for agreeing on and synchronising protocol updates.
  LastKnownBlockVersion-Major = 3;
  LastKnownBlockVersion-Minor = 0;
  LastKnownBlockVersion-Alt = 0;

  # In the Byron era some software versions are also published on the chain.
  # We do this only for Byron compatibility now.
  ApplicationName = "cardano-sl";
  ApplicationVersion = 1;
}
