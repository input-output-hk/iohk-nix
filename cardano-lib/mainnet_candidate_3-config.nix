##########################################################
###############     Mainnet Candidate 3    ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./mainnet_candidate_3-byron-genesis.json;
  ByronGenesisHash = "03c8bb47e437dab5bc78548462cd3d682fe631765a0adf4fa06c88e4115285a9";
  ShelleyGenesisFile = ./mainnet_candidate_3-shelley-genesis.json;
  ShelleyGenesisHash = "17cd10bbb47b31abe44440164409004a14c33e56fe946ae09b9dcd3c9b7c3bbd";

  TestShelleyHardForkAtVersion = 1;

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
