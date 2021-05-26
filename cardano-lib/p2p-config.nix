##########################################################
###############         P2P Testnet        ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./p2p + "/byron-genesis.json";
  ByronGenesisHash = "610d9d846768180f90e547cefcffc7c50cc593d492fb219691f3f90fd37ea9f1";
  ShelleyGenesisFile = ./p2p + "/shelley-genesis.json";
  ShelleyGenesisHash = "9a41ac9f125995c0a558614a8e130fc5530972bb7f6ed11b5003e2cebc48ca1b";
  AlonzoGenesisFile = ./p2p + "/alonzo-genesis.json";
  AlonzoGenesisHash = "06cc024b823b6d20f5dde2faf8de2d895f47983ab584db38ea62111b61038e35";

  ##### Core protocol parameters #####

  # This is the instance of the Ouroboros family that we are running.
  # The node also supports various test and mock instances.
  # "RealPBFT" is the real (ie not mock) (permissive) OBFT protocol, which
  # is what we use on mainnet in Byron era.
  Protocol = "Cardano";

  PBftSignatureThreshold = 1.1;
  # The mainnet does not include the network magic into addresses. Testnets do.
  RequiresNetworkMagic = "RequiresMagic";

  TestShelleyHardForkAtEpoch = 1;
  TestAllegraHardForkAtEpoch = 2;
  TestMaryHardForkAtEpoch = 3;

  MaxKnownMajorProtocolVersion = 2;
  #### LOGGING Debug

  minSeverity = "Debug";

  ##### Update system parameters #####

  # This protocol version number gets used by block producing nodes as part
  # part of the system for agreeing on and synchronising protocol updates.
  LastKnownBlockVersion-Major = 3;
  LastKnownBlockVersion-Minor = 1;
  LastKnownBlockVersion-Alt = 0;

  # In the Byron era some software versions are also published on the chain.
  # We do this only for Byron compatibility now.
  ApplicationName = "cardano-sl";
  ApplicationVersion = 0;
}
