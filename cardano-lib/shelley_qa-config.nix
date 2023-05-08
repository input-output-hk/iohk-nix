##########################################################
###############         Shelley QA         ###############
############### Cardano Node Configuration ###############
##########################################################
{
  ##### Locations #####

  ByronGenesisFile = ./shelley_qa + "/byron-genesis.json";
  ByronGenesisHash = "9325495d3ac7554d4bfaf2392cc3c74676d5add873d6ef8862d7562e660940bf";
  ShelleyGenesisFile = ./shelley_qa + "/shelley-genesis.json";
  ShelleyGenesisHash = "85d1783750753deaa6560158eb85fcb30078ea32a36e12dd8af39168bc053d09";
  AlonzoGenesisFile = ./shelley_qa + "/alonzo-genesis.json";
  AlonzoGenesisHash = "7e94a15f55d1e82d10f09203fa1d40f8eede58fd8066542cf6566008068ed874";
  ConwayGenesisFile = ./shelley_qa + "/conway-genesis.json";
  ConwayGenesisHash = "f28f1c1280ea0d32f8cd3143e268650d6c1a8e221522ce4a7d20d62fc09783e1";

  ##### Core protocol parameters #####

  # This is the instance of the Ouroboros family that we are running.
  # The node also supports various test and mock instances.
  # "RealPBFT" is the real (ie not mock) (permissive) OBFT protocol, which
  # is what we use on mainnet in Byron era.
  Protocol = "Cardano";

  PBftSignatureThreshold = 0.9;
  # The mainnet does not include the network magic into addresses. Testnets do.
  RequiresNetworkMagic = "RequiresMagic";

  TestShelleyHardForkAtEpoch = 2;

  MaxKnownMajorProtocolVersion = 2;
  #### LOGGING Debug

  #minSeverity = "Debug";

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
