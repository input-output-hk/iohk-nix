##########################################################
###############          Testnet           ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./testnet-byron-genesis.json;
  ByronGenesisHash = "96fceff972c2c06bd3bb5243c39215333be6d56aaf4823073dca31afe5038471";
  ShelleyGenesisFile = ./testnet-shelley-genesis.json;
  ShelleyGenesisHash = "9f9afb65aa682b19ff484a00baf7e21dcea965d85f72da85957b8405fd78b41c";


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
  LastKnownBlockVersion-Major = 1;
  LastKnownBlockVersion-Minor = 0;
  LastKnownBlockVersion-Alt = 0;

  # In the Byron era some software versions are also published on the chain.
  # We do this only for Byron compatibility now.
  ApplicationName = "cardano-sl";
  ApplicationVersion = 0;
}
