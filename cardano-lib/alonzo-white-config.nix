##########################################################
###############    Alonzo White Testnet    ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./alonzo-white + "/byron-genesis.json";
  ByronGenesisHash = "4ba13e5d77bb5e243f84f50fd851b4957b43ea93a6f7df5616d6bd25b8f11de0";
  ShelleyGenesisFile = ./alonzo-white + "/shelley-genesis.json";
  ShelleyGenesisHash = "b72001cddc21713dd63d899c1993a5b0728cd909eb261fff0e50d10f46340f1f";
  AlonzoGenesisFile = ./alonzo-white + "/alonzo-genesis.json";
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

  TestShelleyHardForkAtEpoch = 1;
  TestAllegraHardForkAtEpoch = 2;
  TestMaryHardForkAtEpoch = 3;

  TestEnableDevelopmentHardForkEras = true;
  TestEnableDevelopmentNetworkProtocols = true;

  MaxKnownMajorProtocolVersion = 4;
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
