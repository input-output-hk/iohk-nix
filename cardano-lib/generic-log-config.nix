{
  # global filter; messages must have at least this severity to pass:
  minSeverity = "Info";

  TurnOnLogging = true;
  TurnOnLogMetrics = true;

  ViewMode = "SimpleView";

  # global file rotation settings:
  rotation = {
    rpLogLimitBytes = 5000000;
    rpKeepFilesNum  = 10;
    rpMaxAgeHours   = 24;
  };

  Enabled = false;
  Rate = 0;
  Period = "";
  Burst = 0;

  # these backends are initialized:
  setupBackends = [
    "KatipBK"
  ];

  # if not indicated otherwise, then messages are passed to these backends:
  defaultBackends = [
    "KatipBK"
  ];

  # if wanted, the GUI is listening on this port:
  # hasGUI: 12787

  # if wanted, the EKG interface is listening on this port:
  hasEKG = 12788;
  hasPrometheus = [ "127.0.0.1" 12798 ];

  # here we set up outputs of logging in 'katip':
  setupScribes = [ {
    scKind     = "StdoutSK";
    scName     = "stdout";
    scFormat   = "ScText";
    scRotation = null;
  } ];

  # if not indicated otherwise, then log output is directed to this:
  defaultScribes = [
    [
      "StdoutSK"
      "stdout"
    ]
  ];

  #####         Tracing         #####

  # MinimalVerbosity: Minimal level of the rendering of captured items
  # MaximalVerbosity: Maximal level of the rendering of captured items
  # NormalVerbosity: the default level of the rendering of captured items
  TracingVerbosity = "NormalVerbosity";

  # Trace BlockFetch client.
  TraceBlockFetchClient = false;
  # Trace BlockFetch decisions made by the BlockFetch client.
  TraceBlockFetchDecisions = true;
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
  # Trace DNS Resolver messages.
  TraceDNSResolver = false;
  # Trace DNS Subscription messages.
  TraceDNSSubscription = false;
  # Trace error policy resolution.
  TraceErrorPolicy = false;
  # Trace block forging.
  TraceForge = true;
  # Trace IP Subscription messages.
  TraceIpSubscription = false;
  # Trace local ChainSync protocol messages.
  TraceLocalChainSyncProtocol = false;
  # Trace local TxSubmission protocol messages.
  TraceLocalTxSubmissionProtocol = false;
  # Trace local TxSubmission server.
  TraceLocalTxSubmissionServer = false;
  # Trace mempool.
  TraceMempool = true;
  # Trace Mux Events
  TraceMux = false;
  # Trace TxSubmission server (inbound transactions).
  TraceTxInbound = false;
  # Trace TxSubmission client (outbound transactions).
  TraceTxOutbound = false;
  # Trace TxSubmission protocol messages.
  TraceTxSubmissionProtocol = false;

  # more options which can be passed as key-value pairs:
  options = {
    cfokey = {
      value = "Release-1.0.0";
    };
    mapSubtrace = {
      benchmark = {
        contents = [
          "GhcRtsStats"
          "MonotonicClock"
        ];
        subtrace = "ObservableTrace";
      };
      "#ekgview" = {
        contents = [
          [
            {
              tag = "Contains";
              contents = "cardano.epoch-validation.benchmark";
            }
            [
              {
                tag = "Contains";
                contents = ".monoclock.basic.";
              }
            ]
          ]
          [
            {
              tag = "Contains";
              contents = "cardano.epoch-validation.benchmark";
            }
            [
              {
                tag = "Contains";
                contents = "diff.RTS.cpuNs.timed.";
              }
            ]
          ]
          [
            {
              tag = "StartsWith";
              contents = "#ekgview.#aggregation.cardano.epoch-validation.benchmark";
            }
            [
              {
                tag = "Contains";
                contents = "diff.RTS.gcNum.timed.";
              }
            ]
          ]
        ];
        subtrace = "FilterTrace";
      };
      "cardano.epoch-validation.utxo-stats" = {
         # Change the `subtrace` value to `Neutral` in order to log
         # `UTxO`-related messages during epoch validation.
         subtrace = "NoTrace";
      };
      "cardano.node-metrics" = {
         subtrace = "Neutral";
      };
    };
    mapBackends = {
      "cardano.node-metrics" = [
         "EKGViewBK"
         {
            kind = "UserDefinedBK";
            name = "LiveViewBackend";
         }
      ];
      "cardano.node.ChainDB.metrics" = [
         "EKGViewBK"
         {
            kind = "UserDefinedBK";
            name = "LiveViewBackend";
         }
      ];
      "cardano.node.BlockFetchDecision.peers" = [
         {
            kind = "UserDefinedBK";
            name = "LiveViewBackend";
         }
      ];
    };
  };
}
