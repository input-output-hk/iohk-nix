##########################################################
###############          Mainnet           ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./mainnet + "/byron-genesis.json";
  ByronGenesisHash = "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb";
  ConwayGenesisFile = ./mainnet + "/conway-genesis.json";
  ConwayGenesisHash = "15a199f895e461ec0ffc6dd4e4028af28a492ab4e806d39cb674c88f7643ef62";
  ShelleyGenesisFile = ./mainnet + "/shelley-genesis.json";
  ShelleyGenesisHash = "1a3be38bcbb7911969283716ad7aa550250226b76a61fc51cc9a9a35d9276d81";
  AlonzoGenesisFile = ./mainnet + "/alonzo-genesis.json";
  AlonzoGenesisHash = "7e94a15f55d1e82d10f09203fa1d40f8eede58fd8066542cf6566008068ed874";

  ##### Core protocol parameters #####

  # This is the instance of the Ouroboros family that we are running.
  # The node also supports various test and mock instances.
  # "RealPBFT" is the real (ie not mock) (permissive) OBFT protocol, which
  # is what we use on mainnet in Byron era.
  Protocol = "Cardano";

  # The mainnet does not include the network magic into addresses. Testnets do.
  RequiresNetworkMagic = "RequiresNoMagic";
  EnableP2P = true;
  PeerSharing = true;
  TargetNumberOfActivePeers = 20;
  TargetNumberOfEstablishedPeers = 40;
  TargetNumberOfKnownPeers = 150;
  TargetNumberOfRootPeers = 60;
  TraceMempool = false;

  MaxKnownMajorProtocolVersion = 2;

  ##### Update system parameters #####

  # This protocol version number gets used by block producing nodes as part
  # part of the system for agreeing on and synchronising protocol updates.
  LastKnownBlockVersion-Major = 3;
  LastKnownBlockVersion-Minor = 0;
  LastKnownBlockVersion-Alt = 0;
}
