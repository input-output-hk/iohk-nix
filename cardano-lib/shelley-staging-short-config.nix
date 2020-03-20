##########################################################
###############    Shelley Staging Short   ###############
############### Cardano Node Configuration ###############
##########################################################

{
  NodeId = 0;
  NodeHostAddress = "";
  NodePort = 3001;
  Protocol = "RealPBFT";
  GenesisFile = ./shelley-staging-short-genesis.json;
  GenesisHash = "78be790c7c4dec7bd2f690c40296e130fefdd198d1175f2b0e9d7e53675f8779";
  NumCoreNodes = 7;
  RequiresNetworkMagic = "RequiresMagic";

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
