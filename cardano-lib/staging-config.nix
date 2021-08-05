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
  AlonzoGenesisHash = "44d65c89cce5f717e839a86fed28af924252ff46eca5aaf15b47ec0dd6a59578";


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
