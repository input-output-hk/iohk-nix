##########################################################
###############     Mainnet Candidate 2    ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./mainnet_candidate_2-byron-genesis.json;
  ByronGenesisHash = "3f98cee6c4831065617e0494a58a10c4245552820fe0725ce0dbc97127056e15";
  ShelleyGenesisFile = ./mainnet_candidate_2-shelley-genesis.json;
  ShelleyGenesisHash = "42e45c1710975a2c166d38a9a3f971e456f854adf215860918db4d2bda5cc424";

  TestShelleyHardForkAtEpoch = 1;

  ##### Core protocol parameters #####

  # This is the instance of the Ouroboros family that we are running.
  # The node also supports various test and mock instances.
  # "RealPBFT" is the real (ie not mock) (permissive) OBFT protocol, which
  # is what we use on mainnet in Byron era.
  Protocol = "Cardano";

  PBftSignatureThreshold = 0.9;

  # The mainnet candidate utilizes same addresses as mainnet
  RequiresNetworkMagic = "RequiresNoMagic";

  MaxKnownMajorProtocolVersion = 2;


  ##### Update system parameters #####

  # This protocol version number gets used by block producing nodes as part
  # part of the system for agreeing on and synchronising protocol updates.
  LastKnownBlockVersion-Major = 2;
  LastKnownBlockVersion-Minor = 0;
  LastKnownBlockVersion-Alt = 0;

  # In the Byron era some software versions are also published on the chain.
  # We do this only for Byron compatibility now.
  ApplicationName = "cardano-sl";
  ApplicationVersion = 0;
}
