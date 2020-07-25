##########################################################
###############     Mainnet Candidate 2    ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./mainnet_candidate_2-byron-genesis.json;
  ByronGenesisHash = "097eeb739b495b3954e859c0e825bed0a4cb465e059fe01fc80e86fbbc0e63eb";
  ShelleyGenesisFile = ./mainnet_candidate_2-shelley-genesis.json;
  ShelleyGenesisHash = "d7bd78c1306c29540ec44f7a7793c0f7e3551c01a3fd27afef2aa15d885f44a0";

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
  LastKnownBlockVersion-Major = 0;
  LastKnownBlockVersion-Minor = 0;
  LastKnownBlockVersion-Alt = 0;

  # In the Byron era some software versions are also published on the chain.
  # We do this only for Byron compatibility now.
  ApplicationName = "cardano-sl";
  ApplicationVersion = 0;
}
