##########################################################
#######                  Preprod                  ########
####### Cardano Node Block Producer Configuration ########
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./preprod + "/byron-genesis.json";
  ByronGenesisHash = "d4b8de7a11d929a323373cbab6c1a9bdc931beffff11db111cf9d57356ee1937";
  ShelleyGenesisFile = ./preprod + "/shelley-genesis.json";
  ShelleyGenesisHash = "162d29c4e1cf6b8a84f2d692e67a3ac6bc7851bc3e6e4afe64d15778bed8bd86";
  AlonzoGenesisFile = ./preprod + "/alonzo-genesis.json";
  AlonzoGenesisHash = "7e94a15f55d1e82d10f09203fa1d40f8eede58fd8066542cf6566008068ed874";

  ##### Core protocol parameters #####

  # This is the instance of the Ouroboros family that we are running.
  # The node also supports various test and mock instances.
  # "RealPBFT" is the real (ie not mock) (permissive) OBFT protocol, which
  # is what we use on mainnet in Byron era.
  Protocol = "Cardano";

  # The mainnet does not include the network magic into addresses. Testnets do.
  RequiresNetworkMagic = "RequiresMagic";
  EnableP2P = true;
  PeerSharing = false;
  TargetNumberOfActivePeers = 20;
  TargetNumberOfEstablishedPeers = 50;
  TargetNumberOfKnownPeers = 100;
  TargetNumberOfRootPeers = 100;

  ##### Update system parameters #####

  # This protocol version number gets used by block producing nodes as part
  # part of the system for agreeing on and synchronising protocol updates.
  LastKnownBlockVersion-Major = 2;
  LastKnownBlockVersion-Minor = 0;
  LastKnownBlockVersion-Alt = 0;
}
