##########################################################
###############         Marlowe Dev        ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./marlowe-dev + "/byron-genesis.json";
  ByronGenesisHash = "402e3ed3b4502bd79cc97931d7e9ca1caadebf7de7835699e7032589501353aa";
  ShelleyGenesisFile = ./marlowe-dev + "/shelley-genesis.json";
  ShelleyGenesisHash = "b74b0d68de7ee0c8aae997971ecd8b1f19966bdbde9e395d5cb0820ef8287d56";
  AlonzoGenesisFile = ./marlowe-dev + "/alonzo-genesis.json";
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
