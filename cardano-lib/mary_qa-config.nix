##########################################################
###############         Mary QA            ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./mary_qa-byron-genesis.json;
  ByronGenesisHash = "992aac83a03ba7b8ca50fd3af29c527dfcfa4a8b3c1a1273c698d95fd7c519d3";
  ShelleyGenesisFile = ./mary_qa-shelley-genesis.json;
  ShelleyGenesisHash = "d80b878ca5fbaa54977eaa07d18b31afdca5527b231e93a5a34437f0f81b36d6";

  ##### Core protocol parameters #####

  # This is the instance of the Ouroboros family that we are running.
  # The node also supports various test and mock instances.
  # "RealPBFT" is the real (ie not mock) (permissive) OBFT protocol, which
  # is what we use on mainnet in Byron era.
  Protocol = "Cardano";

  PBftSignatureThreshold = 0.9;
  # The mainnet does not include the network magic into addresses. Testnets do.
  RequiresNetworkMagic = "RequiresMagic";

  TestShelleyHardForkAtEpoch = 1;
  TestAllegraHardForkAtEpoch = 2;
  #TestMaryHardForkAtEpoch = 3;

  MaxKnownMajorProtocolVersion = 2;
  #### LOGGING Debug

  minSeverity = "Debug";

  ##### Update system parameters #####

  # This protocol version number gets used by block producing nodes as part
  # part of the system for agreeing on and synchronising protocol updates.
  LastKnownBlockVersion-Major = 3;
  LastKnownBlockVersion-Minor = 0;
  LastKnownBlockVersion-Alt = 0;

  # In the Byron era some software versions are also published on the chain.
  # We do this only for Byron compatibility now.
  ApplicationName = "cardano-sl";
  ApplicationVersion = 0;
}
