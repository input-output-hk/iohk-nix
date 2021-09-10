##########################################################
###############         P2P Testnet        ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./p2p + "/byron-genesis.json";
  ByronGenesisHash = "414e27b56b4b147e40bd95cc5552a2c97043c04e3de8b4c7ea5fc90fce25a68e";
  ShelleyGenesisFile = ./p2p + "/shelley-genesis.json";
  ShelleyGenesisHash = "a0e8e5520ab7c452c4e36020ded12c791a10abd7ca25c083af6149fe269ddb67";
  AlonzoGenesisFile = ./p2p + "/alonzo-genesis.json";
  AlonzoGenesisHash = "7e94a15f55d1e82d10f09203fa1d40f8eede58fd8066542cf6566008068ed874";

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
  TestAlonzoHardForkAtEpoch = 4;

  ### P2P

  EnableP2P = true;
  TestEnableDevelopmentNetworkProtocols = true;
  TraceInboundGovernorCounters = true;

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
