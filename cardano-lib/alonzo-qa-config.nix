##########################################################
###############     Alonzo Blue Testnet    ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./alonzo-qa + "/byron-genesis.json";
  ByronGenesisHash = "78d34abac0fb75dfc090d7780a38f72011feecb1ff69a888fbb0d9b8128c1951";
  ShelleyGenesisFile = ./alonzo-qa + "/shelley-genesis.json";
  ShelleyGenesisHash = "dcefde8004a9861c42860aa4637114a0b2303cba6635ea28c1976e792cbc9f74";
  AlonzoGenesisFile = ./alonzo-qa + "/alonzo-genesis.json";
  AlonzoGenesisHash = "c7cdcccbc6adec169f74784de720db8414665591d87498b513266d41680c587f";

  ##### Core protocol parameters #####

  # This is the instance of the Ouroboros family that we are running.
  # The node also supports various test and mock instances.
  # "RealPBFT" is the real (ie not mock) (permissive) OBFT protocol, which
  # is what we use on mainnet in Byron era.
  Protocol = "Cardano";

  PBftSignatureThreshold = 1.1;
  # The mainnet does not include the network magic into addresses. Testnets do.
  RequiresNetworkMagic = "RequiresMagic";

  TestShelleyHardForkAtEpoch = 1;
  TestAllegraHardForkAtEpoch = 2;
  TestMaryHardForkAtEpoch = 3;
  TestAlonzoHardForkAtEpoch = 4;

  TestEnableDevelopmentHardForkEras = true;
  TestEnableDevelopmentNetworkProtocols = true;

  MaxKnownMajorProtocolVersion = 5;
  #### LOGGING Debug

  minSeverity = "Debug";

  ##### Update system parameters #####

  # This protocol version number gets used by block producing nodes as part
  # part of the system for agreeing on and synchronising protocol updates.
  LastKnownBlockVersion-Major = 5;
  LastKnownBlockVersion-Minor = 1;
  LastKnownBlockVersion-Alt = 0;

  # In the Byron era some software versions are also published on the chain.
  # We do this only for Byron compatibility now.
  ApplicationName = "cardano-sl";
  ApplicationVersion = 0;
}
