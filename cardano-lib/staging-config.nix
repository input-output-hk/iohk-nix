##########################################################
###############          Staging           ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./staging-byron-genesis.json;
  ByronGenesisHash = "c6a004d3d178f600cd8caa10abbebe1549bef878f0665aea2903472d5abf7323";
  ShelleyGenesisFile = ./staging-shelley-genesis.json;
  ShelleyGenesisHash = "d139ede2f87ac80a5a27bdf59f0a17afbde44469129af3abd8b77915683b70d0";


  ##### Core protocol parameters #####

  # This is the instance of the Ouroboros family that we are running.
  # The node also supports various test and mock instances.
  # "RealPBFT" is the real (ie not mock) (permissive) OBFT protocol, which
  # is what we use on mainnet in Byron era.
  Protocol = "Cardano";

  # The mainnet does not include the network magic into addresses. Testnets do.
  RequiresNetworkMagic = "RequiresNoMagic";

  MaxKnownMajorProtocolVersion = 2;

  ##### Update system parameters #####

  # This protocol version number gets used by by block producing nodes as part
  # part of the system for agreeing on and synchronising protocol updates.
  LastKnownBlockVersion-Major = 1;
  LastKnownBlockVersion-Minor = 0;
  LastKnownBlockVersion-Alt = 0;

  # In the Byron era some software versions are also published on the chain.
  # We do this only for Byron compatibility now.
  ApplicationName = "cardano-sl";
  ApplicationVersion = 1;
}
