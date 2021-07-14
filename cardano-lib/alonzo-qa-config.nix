##########################################################
###############     Alonzo Blue Testnet    ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./alonzo-qa + "/byron-genesis.json";
  ByronGenesisHash = "427e57feb8a142ad02281249f53d1e810fb30b4480a247b7bf1d7b3ea042c450";
  ShelleyGenesisFile = ./alonzo-qa + "/shelley-genesis.json";
  ShelleyGenesisHash = "76b558a3fa118b45077d96e222d8bfe60122fda2fb81b39025c64012684c4260";
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
