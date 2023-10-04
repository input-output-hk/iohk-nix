##########################################################
###############          Testnet           ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./testnet + "/byron-genesis.json";
  ByronGenesisHash = "96fceff972c2c06bd3bb5243c39215333be6d56aaf4823073dca31afe5038471";
  ShelleyGenesisFile = ./testnet + "/shelley-genesis.json";
  ShelleyGenesisHash = "849a1764f152e1b09c89c0dfdbcbdd38d711d1fec2db5dfa0f87cf2737a0eaf4";
  AlonzoGenesisFile = ./testnet + "/alonzo-genesis.json";
  AlonzoGenesisHash = "7e94a15f55d1e82d10f09203fa1d40f8eede58fd8066542cf6566008068ed874";
  ConwayGenesisFile = ./mainnet + "/conway-genesis.json";
  ConwayGenesisHash = "f7d46bdd3b3c8caf38351c4eef3346a89241707270be0d6106e8a407db294cc6";

  ##### Core protocol parameters #####

  # This is the instance of the Ouroboros family that we are running.
  # The node also supports various test and mock instances.
  # "RealPBFT" is the real (ie not mock) (permissive) OBFT protocol, which
  # is what we use on mainnet in Byron era.
  Protocol = "Cardano";

  # The mainnet does not include the network magic into addresses. Testnets do.
  RequiresNetworkMagic = "RequiresMagic";

  MaxKnownMajorProtocolVersion = 2;

  ##### Update system parameters #####

  # This protocol version number gets used by block producing nodes as part
  # part of the system for agreeing on and synchronising protocol updates.
  LastKnownBlockVersion-Major = 3;
  LastKnownBlockVersion-Minor = 0;
  LastKnownBlockVersion-Alt = 0;
}
