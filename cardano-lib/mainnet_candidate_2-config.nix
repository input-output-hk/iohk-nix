##########################################################
###############     Mainnet Candidate 2    ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./mainnet_candidate_2-byron-genesis.json;
  ByronGenesisHash = "c15c651a112fd3269f3fc5b0025397d76293f01c746ca33a7aa6b538f5c12ea4";
  ShelleyGenesisFile = ./mainnet_candidate_2-shelley-genesis.json;
  ShelleyGenesisHash = "973276af2a37b04b6874c5c6b0ead6645fea126536f7fd2edb9892975fbe9abe";

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
