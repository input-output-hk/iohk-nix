##########################################################
###############     Alonzo Blue Testnet    ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./alonzo-qa + "/byron-genesis.json";
  ByronGenesisHash = "ada89200d9fabadc8cc857d2a151f424b039738351a72219bf79095f513f1957";
  ShelleyGenesisFile = ./alonzo-qa + "/shelley-genesis.json";
  ShelleyGenesisHash = "db3b9a5b1d3b10cd06d09ba215737e69842e6557276a66fac2b1b2e9c5ac25c1";
  AlonzoGenesisFile = ./alonzo-qa + "/alonzo-genesis.json";
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

  TestEnableDevelopmentHardForkEras = true;
  TestEnableDevelopmentNetworkProtocols = true;

  MaxKnownMajorProtocolVersion = 2;
  #### LOGGING Debug

  minSeverity = "Debug";

  ##### Update system parameters #####

  # This protocol version number gets used by block producing nodes as part
  # part of the system for agreeing on and synchronising protocol updates.
  LastKnownBlockVersion-Major = 5;
  LastKnownBlockVersion-Minor = 1;
  LastKnownBlockVersion-Alt = 0;

  # In the Byron era some software versions are also published on the chain.
  # We do this only for Byron compatibility now.
  ApplicationName = "cardano-sl";
  ApplicationVersion = 0;
}
