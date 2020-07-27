##########################################################
###############      Mainnet Candidate     ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./mainnet_candidate-byron-genesis.json;
  ByronGenesisHash = "ff67af93b33ffbce18195557a8acc4aa6b904ebe6be4a736edee5cb7f44bc7cb";
  ShelleyGenesisFile = ./mainnet_candidate-shelley-genesis.json;
  ShelleyGenesisHash = "ec3bd2959cd65b894f6abd12d4eff32da9778eed33f0777017f999e7b82efadd";


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
  LastKnownBlockVersion-Major = 0;
  LastKnownBlockVersion-Minor = 0;
  LastKnownBlockVersion-Alt = 0;

  # In the Byron era some software versions are also published on the chain.
  # We do this only for Byron compatibility now.
  ApplicationName = "cardano-sl";
  ApplicationVersion = 0;
}
