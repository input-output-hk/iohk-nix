{
  ##### The Basics #####

  # Enable or disable logging overall
  TurnOnLogging = true;

  # Enable the collection of various OS metrics such as memory and CPU use.
  # These metrics can be directed to the logs or monitoring backends.
  TurnOnLogMetrics = true;

  # Use the modern tracing system instead of the legacy tracing system.
  UseTraceDispatcher = true;

  ##### TODO: TITLE #####

  # Match the metrics prefix of the legacy tracing system to minimize breaking
  # changes.
  TraceOptionMetricsPrefix = "cardano.node.metrics.";

  # Optional node name.  Defaults to hostname if left unset. Ideally this is
  # set in downstream code where the node's name is known.
  # TraceOptionNodeName =

  # The frequency of peer messages.
  TraceOptionPeerFrequency = 2000;

  # The frequency of resource messages.
  TraceOptionResourceFrequency = 1000;

  # TODO: Fix up following calcs in description
  #
  # Queue size control:
  # In case of a missing forwarding service consumer, traces messages will be
  # buffered. This mitigates short forwarding interruptions, or delays at
  # startup time.
  #
  # The queue capacity should thus correlate to the expected log lines per
  # second given a particular tracing configuration - to avoid unnecessarily
  # increasing memory footprint.
  #
  # The default values here are chosen to accommodate verbose tracing output
  # (i.e., buffering 1min worth of trace data given ~32 messages per second). A
  # config that results in less than 5 msgs per second should also provide
  # TraceOptionForwarder queue size values considerably lower. The
  # `disconnQueueSize` is the hard limit in that case.
  #
  # The queue sizes tie in with the max number of trace objects cardano-tracer
  # requests periodically, the default for that being 100. Here, the basic
  # queue can hold enough traces for 10 subsequent polls.
  TraceOptionForwarder = {
    connQueueSize = 64;
    disconnQueueSize = 128;
  };

  TraceOptions = {
    "" = {
      backends = [
        "Stdout HumanFormatColoured"
        "EKGBackend"
        "Forwarder"
      ];

      detail = "DNormal";
      severity = "Notice";
    };

    "BlockFetch.Decision" = {
      severity = "Silence";
    };

    "ChainDB" = {
      severity = "Info";
    };

    "ChainDB.AddBlockEvent.AddBlockValidation" = {
      severity = "Silence";
    };

    "ChainSync.Client" = {
      severity = "Warning";
    };

    "Net.ConnectionManager.Remote" = {
      severity = "Info";
    };

    "Net.Subscription.DNS" = {
      severity = "Info";
    };

    "Startup.DiffusionInit" = {
      severity = "Info";
    };

    "Net.ErrorPolicy" = {
      severity = "Info";
    };

    "Forge.Loop" = {
      severity = "Info";
    };

    "Forge.StateInfo" = {
      severity = "Info";
    };

    "Net.InboundGovernor.Remote" = {
      severity = "Info";
    };

    "Net.Subscription.IP" = {
      severity = "Info";
    };

    "Net.ErrorPolicy.Local" = {
      severity = "Info";
    };

    "Mempool" = {
      severity = "Info";
    };

    "Net.Mux.Remote" = {
      severity = "Info";
    };

    "Net.InboundGovernor" = {
      severity = "Warning";
    };

    "Net.PeerSelection" = {
      severity = "Silence";
    };

    "Net.ConnectionManager.Remote.ConnectionManagerCounters" = {
      severity = "Silence";
    };

    "Resources" = {
      severity = "Silence";
    };

    "ChainDB.AddBlockEvent.AddedBlockToQueue" = {
      maxFrequency = 2.0;
    };

    "ChainDB.AddBlockEvent.AddedBlockToVolatileDB" = {
      maxFrequency = 2.0;
    };

    "ChainDB.AddBlockEvent.AddBlockValidation.ValidCandidate" = {
      maxFrequency = 2.0;
    };

    "ChainDB.CopyToImmutableDBEvent.CopiedBlockToImmutableDB" = {
      maxFrequency = 2.0;
    };

    "BlockFetch.Client.CompletedBlockFetch" = {
      maxFrequency = 2.0;
    };
  };
}
