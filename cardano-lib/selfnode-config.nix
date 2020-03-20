##########################################################
###############          Selfnode          ###############
############### Cardano Node Configuration ###############
##########################################################

{
  NodeId = 0;
  Protocol = "RealPBFT";
  GenesisFile = ./selfnode-genesis.json;
  NumCoreNodes = 1;
  PBftSignatureThreshold = 1000000000000000;
  RequiresNetworkMagic = "RequiresNoMagic";

  ##### Network Time Parameters #####

  ResponseTimeout = 30000000;
  PollDelay = 1800000000;
  Servers = [
    "0.pool.ntp.org"
    "2.pool.ntp.org"
    "3.pool.ntp.org"
  ];

  #####    Update Parameters    #####

  ApplicationName = "cardano-sl";
  ApplicationVersion = 0;
  LastKnownBlockVersion-Major = 0;
  LastKnownBlockVersion-Minor = 0;
  LastKnownBlockVersion-Alt = 0;

  MemPoolLimitTx = 200;
  AssetLockedSrcAddress = [];

  CacheParameter = 500;
  MessageCacheTimeout = 30;

  NetworkDiameter = 18;
  RecoveryHeadersMessage = 2200;
  StreamWindow = 2048;
  NonCriticalCQBootstrap = 0.95;
  NonCriticalCQ = 0.8;
  CriticalCQBootstrap = 0.8888;
  CriticalCQ = 0.654321;
  CriticalForkThreshold = 3;
  FixedTimeCQ = 3600;

  SlotLength = 20000;
  NetworkConnectionTimeout = 15000;
  HandshakeTimeout = 30000;
}
