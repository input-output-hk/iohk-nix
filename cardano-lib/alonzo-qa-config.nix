##########################################################
###############     Alonzo Blue Testnet    ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./alonzo-qa + "/byron-genesis.json";
  ByronGenesisHash = "0f192fec1b02283025cef1f8936224e69b37cff565c54ab59c3a88468c6811be";
  ShelleyGenesisFile = ./alonzo-qa + "/shelley-genesis.json";
  ShelleyGenesisHash = "0103899136eacc050911157a6cb4047010da72442e53a673afbc58c161353c27";
  AlonzoGenesisFile = ./alonzo-qa + "/alonzo-genesis.json";
  AlonzoGenesisHash = "dfdbcb0e55a2e389e69e703797b945b52aae500db33fcd2301f0c328afe4f7e2";

  ##### Core protocol parameters #####

  # This is the instance of the Ouroboros family that we are running.
  # The node also supports various test and mock instances.
  # "RealPBFT" is the real (ie not mock) (permissive) OBFT protocol, which
  # is what we use on mainnet in Byron era.
  Protocol = "Cardano";

  PBftSignatureThreshold = 1.1;
  # The mainnet does not include the network magic into addresses. Testnets do.
  RequiresNetworkMagic = "RequiresMagic";

  TestShelleyHardForkAtEpoch = 0;
  TestAllegraHardForkAtEpoch = 0;
  TestMaryHardForkAtEpoch = 0;
  TestAlonzoHardForkAtEpoch = 0;

  TestEnableDevelopmentHardForkEras = true;
  TestEnableDevelopmentNetworkProtocols = true;

  MaxKnownMajorProtocolVersion = 5;
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
