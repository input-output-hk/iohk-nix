##########################################################
###############          Testnet           ###############
############### Cardano Node Configuration ###############
##########################################################

{
  NodeId = 0;
  NodeHostAddress = "";
  NodePort = 7000;
  Protocol = "RealPBFT";
  GenesisHash = "96fceff972c2c06bd3bb5243c39215333be6d56aaf4823073dca31afe5038471";
  NumCoreNodes = 7;
  RequiresNetworkMagic = "RequiresMagic";
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
