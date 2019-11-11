##########################################################
###############          Mainnet           ###############
############### Cardano Node Configuration ###############
##########################################################

{
  NodeId = 0;
  NodeHostAddress = "";
  NodePort = 7000;
  Protocol = "RealPBFT";
  GenesisHash = "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb";
  NumCoreNodes = 1;
  RequiresNetworkMagic = "RequiresNoMagic";
  PBftSignatureThreshold = 0.5;

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
  ApplicationVersion = 1;
  LastKnownBlockVersion-Major = 0;
  LastKnownBlockVersion-Minor = 2;
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

  CA-Organization = "Input Output HK";
  CA-CommonName = "Cardano SL Self-Signed Root CA";
  CA-ExpiryDays = 3600;
  CA-AltDNS = [];

  Server-Organization = "Input Output HK";
  Server-CommonName = "Cardano SL Server";
  Server-ExpiryDays = 3600;
  Server-AltDNS = [ "localhost"
                    "localhost.localdomain"
                    "127.0.0.1"
                    "::1"
                  ];

  Wallet-Organization = "Input Output HK";
  Wallet-CommonName = "Daedalus Wallet";
  Wallet-ExpiryDays = 3600;
  Wallet-AltDNS = [];
}
