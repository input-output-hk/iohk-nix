##########################################################
###############    Alonzo Purple Testnet   ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./alonzo-purple + "/byron-genesis.json";
  ByronGenesisHash = "6d58e8c626cb355ac2a4b040e1adb8266753a69f63593274e4558d6bd0c41eac";
  ShelleyGenesisFile = ./alonzo-purple + "/shelley-genesis.json";
  ShelleyGenesisHash = "b143c75727f4b2fb372db713e719f9b958bb428e305a668bda6190443db4c191";
  AlonzoGenesisFile = ./alonzo-purple + "/alonzo-genesis.json";
  AlonzoGenesisHash = "44d65c89cce5f717e839a86fed28af924252ff46eca5aaf15b47ec0dd6a59578";

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
