{

  # Enable or disable logging overall
  TurnOnLogging = true;

  # Enable the collection of various OS metrics such as memory and CPU use.
  # These metrics can be directed to the logs or monitoring backends.
  TurnOnLogMetrics = true;

  # Global logging severity filter. Messages must have at least this severity to
  # pass. Typical values would be Warning, Notice, Info or Debug.
  minSeverity = "Info";

  # Log items can be rendered with more or less verbose detail.
  # The verbosity can be: MinimalVerbosity, NormalVerbosity
  TracingVerbosity = "NormalVerbosity";

  # The system supports a number of backends for logging and monitoring.
  # This setting lists the the backends that will be available to use in the
  # configuration below. The logging backend is called Katip. Also enable the EKG
  # backend if you want to use the EKG or Prometheus monitoring interfaces.
  setupBackends = [
    "KatipBK"
    # EKGViewBK
  ];

  # This specifies the default backends that trace output is sent to if it
  # is not specifically configured to be sent to other backends.
  defaultBackends = [
    "KatipBK"
  ];

  # EKG is a simple metrics monitoring system. Uncomment the following to listen
  # on the given local port and point your web browser to http://localhost:12788/
  # for a live view. The same URL can also serve JSON output.
  hasEKG = 12788;

  # The Prometheus monitoring system can also be used. Uncomment the following
  # to listen on the given port
  hasPrometheus = [ "127.0.0.1" 12798 ];

  # For the Katip logging backend we must set up outputs (called scribes)
  # The available types of scribe are:
  #   FileSK for files
  #   StdoutSK/StdoutSK for stdout/stderr
  #   JournalSK for systemd's journal system
  #   DevNullSK
  # The scribe output format can be ScText or ScJson. Log rotation settings can
  # be specified in the defaults below or overidden on a per-scribe basis here.
  setupScribes = [ {
    scKind     = "StdoutSK";
    scName     = "stdout";
    scFormat   = "ScText";
    scRotation = null;
  } ];

  # For the Katip logging backend this specifies the default scribes that trace
  # output is sent to if it is not configured to be sent to other scribes.
  defaultScribes = [
    [
      "StdoutSK"
      "stdout"
    ]
  ];

  # The default file rotation settings for katip scribes, unless overridden
  # in the setupScribes above for specific scribes.
  rotation = {
    rpLogLimitBytes = 5000000;
    rpKeepFilesNum  = 10;
    rpMaxAgeHours   = 24;
  };


  ##### Coarse grained logging control #####

  # Trace output from whole subsystems can be enabled/disabled using the following
  # settings. This provides fairly coarse grained control, but it is relatively
  # efficient at filtering out unwanted trace output.

  # Trace BlockFetch client.
  TraceBlockFetchClient = false;

  # Trace BlockFetch decisions made by the BlockFetch client.
  TraceBlockFetchDecisions = false;

  # Trace BlockFetch protocol messages.
  TraceBlockFetchProtocol = false;

  # Serialised Trace BlockFetch protocol messages.
  TraceBlockFetchProtocolSerialised = false;

  # Trace BlockFetch server.
  TraceBlockFetchServer = false;

  # Verbose tracer of ChainDB
  TraceChainDb = true;

  # Trace ChainSync client.
  TraceChainSyncClient = false;

  # Trace ChainSync server (blocks).
  TraceChainSyncBlockServer = false;

  # Trace ChainSync server (headers)
  TraceChainSyncHeaderServer = false;

  # Trace ChainSync protocol messages.
  TraceChainSyncProtocol = false;

  # Trace connection manager
  TraceConnectionManager = true;

  # Trace diffusion initialization messages
  TraceDiffusionInitialization = true;

  # Trace DNS Resolver messages.
  TraceDNSResolver = true;

  # Trace DNS Subscription messages.
  TraceDNSSubscription = true;

  # Trace error policy resolution.
  TraceErrorPolicy = true;

  # Trace local error policy resolution.
  TraceLocalErrorPolicy = true;

  # Trace block forging.
  TraceForge = true;

  # Trace Handshake protocol messages.
  TraceHandshake = false;

  TraceInboundGovernor = true;

  # Trace IP Subscription messages.
  TraceIpSubscription = true;

  # Trace ledger peers.
  TraceLedgerPeers = true;

  # Trace local ChainSync protocol messages.
  TraceLocalChainSyncProtocol = false;

  # Trace local Handshake protocol messages.
  TraceLocalHandshake = false;

  # Trace local root peers
  TraceLocalRootPeers = true;

  # Trace local TxSubmission protocol messages.
  TraceLocalTxSubmissionProtocol = false;

  # Trace local TxSubmission server.
  TraceLocalTxSubmissionServer = false;

  # Trace mempool.
  TraceMempool = true;

  # Trace Mux Events
  TraceMux = false;

  # Trace peer selection
  TracePeerSelection = true;

  # Trace peer selection actions (demotion / protmotion between cold / warm and
  # hot peers).
  TracePeerSelectionActions = true;

  # Trace public root peers
  TracePublicRootPeers = true;

  # Trace server
  TraceServer = true;

  # Trace TxSubmission server (inbound transactions).
  TraceTxInbound = false;

  # Trace TxSubmission client (outbound transactions).
  TraceTxOutbound = false;

  # Trace TxSubmission protocol messages.
  TraceTxSubmissionProtocol = false;


  ##### Fine grained logging control #####

  # It is also possible to have more fine grained control over filtering of
  # trace output, and to match and route trace output to particular backends.
  # This is less efficient than the coarse trace filters above but provides
  # much more precise control.

  options = {

    # This routes metrics matching specific names to particular backends.
    # This overrides the defaultBackends listed above. And note that it is
    # and override and not an extension so anything matched here will not
    # go to the default backend, only to the explicitly listed backends.
    mapBackends = {
      "cardano.node.metrics" = [ "EKGViewBK" ];
      "cardano.node.resources" = [ "EKGViewBK" ];
    };

    # This section is more expressive still, and needs to be properly documented.
    mapSubtrace = {
      "cardano.node.metrics" = {
         subtrace = "Neutral";
      };
    };
  };
}
