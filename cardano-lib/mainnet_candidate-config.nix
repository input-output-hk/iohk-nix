##########################################################
###############      Mainnet Candidate     ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./mainnet_candidate-byron-genesis.json;
  ByronGenesisHash = "33233fcae89c68fd829520d93990812c2b152b840e2e2aeaeaa6b69eccce25c8";
  ShelleyGenesisFile = ./mainnet_candidate-shelley-genesis.json;
  ShelleyGenesisHash = "5dc651631a9ddd8f387f00748d899ebaae156541dba1c65965bb8ab82d49292a";


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
  LastKnownBlockVersion-Major = 2;
  LastKnownBlockVersion-Minor = 0;
  LastKnownBlockVersion-Alt = 0;

  # In the Byron era some software versions are also published on the chain.
  # We do this only for Byron compatibility now.
  ApplicationName = "cardano-sl";
  ApplicationVersion = 0;
}
