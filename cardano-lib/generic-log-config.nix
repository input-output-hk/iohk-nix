{

  # Enable or disable logging overall
  TurnOnLogging = true;

  # Enable the collection of various OS metrics such as memory and CPU use.
  # These metrics can be directed to the logs or monitoring backends.
  TurnOnLogMetrics = true;

  # Use the modern tracing system
  UseTraceDispatcher = true;

  # Set severities for the tracing namespace
  TraceOptionSeverity = [

    # Globally, i.e. default:
    { ns = "";                      severity = "Notice"; }

    # For sub-hierarchies of Cardano.Node.XXX:
    { ns = "Node.ChainDB";          severity = "Info"; }
    { ns = "Node.AcceptPolicy";     severity = "Info"; }
    { ns = "Node.DNSResolver";      severity = "Info"; }
    { ns = "Node.DNSSubscription";  severity = "Info"; }
    { ns = "Node.DiffusionInit";    severity = "Info"; }
    { ns = "Node.ErrorPolicy";      severity = "Info"; }
    { ns = "Node.Forge";            severity = "Info"; }
    { ns = "Node.IpSubscription";   severity = "Info"; }
    { ns = "Node.LocalErrorPolicy"; severity = "Info"; }
    { ns = "Node.Mempool";          severity = "Info"; }
    { ns = "Node.Resources";        severity = "Info"; }
  ];

  # Global default detail level
  TraceOptionDetail = [
    { ns = ""; detail = "DNormal"; }
  ];

  # Global default backends
  TraceOptionBackend = [
    {
      ns = "";
      backends = [
        "Stdout MachineFormat";
        "EKGBackend";
      ]
    }
  ];

  TraceOptionLimiter = [
    {
      ns = "Node.ChainDB.AddBlockEvent.AddedBlockToQueue";
      limiterName = "AddedBlockToQueueLimiter";
      limiterFrequency = 2;
    }
    {
      ns = "Node.ChainDB.AddBlockEvent.AddedBlockToVolatileDB";
      limiterName = "AddedBlockToVolatileDBLimiter";
      limiterFrequency = 2;
    }
    {
      ns = "Node.ChainDB.CopyToImmutableDBEvent.CopiedBlockToImmutableDB";
      limiterName = "CopiedBlockToImmutableDBLimiter";
      limiterFrequency = 2;
    }
    {
      ns = "Node.ChainDB.AddBlockEvent.AddBlockValidation.ValidCandidate";
      limiterName = "ValidCandidateLimiter";
      limiterFrequency = 2;
    }
    {
      ns = "Node.BlockFetchClient.CompletedBlockFetch";
      limiterName = "CompletedBlockFetchLimiter";
      limiterFrequency = 2;
    }
  ];

  TraceOptionForwarder = {
    address = {
      filePath = "forwarder.sock";
    };
    mode = "Initiator";
  };

  TraceOptionPeerFrequency = 2000;
  TraceOptionResourceFrequency = 5000;
}
