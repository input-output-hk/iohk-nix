##########################################################
###############     Mainnet Candidate 3    ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./mainnet_candidate_4-byron-genesis.json;
  ByronGenesisHash = "406bd7edfa14db46edb367d18f5b3dba8d0c626b7c1c19f283867a70d05945c9";
  ShelleyGenesisFile = ./mainnet_candidate_4-shelley-genesis.json;
  ShelleyGenesisHash = "ef8a74ab8587db4c95a7b98cd15406faf7044d9ff47b977f54053f7ad4fd9e59";

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

  TestShelleyHardForkAtEpoch = 1;

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
