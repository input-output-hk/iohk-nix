##########################################################
###############       Shelley Staging      ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./shelley_staging-byron-genesis.json;
  ByronGenesisHash = "82995abf3e0e0f8ab9a6448875536a1cba305f3ddde18cd5ff54c32d7a5978c6";
  ShelleyGenesisFile = ./shelley_staging-shelley-genesis.json;
  ShelleyGenesisHash = "0e7f08a8c79674cf91c64f8dc25557b046fc23c487cd6fc8454ce5357a1d805e";


  ##### Core protocol parameters #####

  # This is the instance of the Ouroboros family that we are running.
  # The node also supports various test and mock instances.
  # "RealPBFT" is the real (ie not mock) (permissive) OBFT protocol, which
  # is what we use on mainnet in Byron era.
  Protocol = "Cardano";

  # The mainnet does not include the network magic into addresses. Testnets do.
  RequiresNetworkMagic = "RequiresMagic";

  MaxKnownMajorProtocolVersion = 2;

  # Bounds the proportion of the latest K
  # blocks which is allowed to be signed by any single key.
  PBftSignatureThreshold = 0.5;

  ##### Update system parameters #####

  # This protocol version number gets used by block producing nodes as part
  # part of the system for agreeing on and synchronising protocol updates.
  LastKnownBlockVersion-Major = 2;
  LastKnownBlockVersion-Minor = 0;
  LastKnownBlockVersion-Alt = 0;

  # In the Byron era some software versions are also published on the chain.
  # We do this only for Byron compatibility now.
  ApplicationName = "cardano-sl";
  ApplicationVersion = 0;
}
