##########################################################
###############       Marlowe Pioneers     ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./marlowe-pioneers + "/byron-genesis.json";
  ByronGenesisHash = "0622f8373c0569490bce2b0fd5593368bd65fe72ee64785acb58adb2f69036a9";
  ShelleyGenesisFile = ./marlowe-pioneers + "/shelley-genesis.json";
  ShelleyGenesisHash = "bfa694eb98f144b36697a6d8998a44090dbbc825cfeb934fa860f0f370671501";
  AlonzoGenesisFile = ./marlowe-pioneers + "/alonzo-genesis.json";
  AlonzoGenesisHash = "559117c7e0e577fe7f67df03b661a87b18f4fc935e90f7509e4f3052366ca914";

  ##### Core protocol parameters #####

  # This is the instance of the Ouroboros family that we are running.
  # The node also supports various test and mock instances.
  # "RealPBFT" is the real (ie not mock) (permissive) OBFT protocol, which
  # is what we use on mainnet in Byron era.
  Protocol = "Cardano";

  PBftSignatureThreshold = 1;
  # The mainnet does not include the network magic into addresses. Testnets do.
  RequiresNetworkMagic = "RequiresMagic";
  #### LOGGING Debug

  minSeverity = "Debug";

  ##### Update system parameters #####

  # This protocol version number gets used by block producing nodes as part
  # part of the system for agreeing on and synchronising protocol updates.
  LastKnownBlockVersion-Major = 6;
  LastKnownBlockVersion-Minor = 0;
  LastKnownBlockVersion-Alt = 0;

  # In the Byron era some software versions are also published on the chain.
  # We do this only for Byron compatibility now.
  ApplicationName = "cardano-sl";
  ApplicationVersion = 0;
}
