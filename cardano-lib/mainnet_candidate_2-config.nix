##########################################################
###############     Mainnet Candidate 2    ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./mainnet_candidate_2-byron-genesis.json;
  ByronGenesisHash = "7e5a9acd813b0bbc059ab50a33fc7441efb1fc887bf8d84ee87d84c58e3d4636";
  ShelleyGenesisFile = ./mainnet_candidate_2-shelley-genesis.json;
  ShelleyGenesisHash = "d1738667a9f7ca6ceb28889e4a713bf146b98123dc06937438c3bb7c64a391e7";


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

  # This protocol version number gets used by by block producing nodes as part
  # part of the system for agreeing on and synchronising protocol updates.
  LastKnownBlockVersion-Major = 1;
  LastKnownBlockVersion-Minor = 0;
  LastKnownBlockVersion-Alt = 0;

  # In the Byron era some software versions are also published on the chain.
  # We do this only for Byron compatibility now.
  ApplicationName = "cardano-sl";
  ApplicationVersion = 0;
}
