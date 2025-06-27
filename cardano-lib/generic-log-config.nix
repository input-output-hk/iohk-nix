{
  # Enable or disable logging overall
  TurnOnLogging = true;

  # Enable the collection of various OS metrics such as memory and CPU use.
  # These metrics can be directed to the logs or monitoring backends.
  TurnOnLogMetrics = true;

  # Use the modern tracing system instead of the legacy tracing system.
  UseTraceDispatcher = true;

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

  # Queue size control:
  # In case of a missing forwarding service consumer, trace messages will be
  # buffered. This mitigates short forwarding interruptions, or delays at
  # startup time.
  #
  # The queue capacity should thus correlate to the expected log lines per
  # second given a particular tracing configuration to avoid unnecessarily
  # increasing memory footprint.
  #
  # The maxReconnectDelay config option specifies the maximum delay in seconds
  # between (re-)connection attempts of a forwarder.
  TraceOptionForwarder = {
    connQueueSize = 64;
    disconnQueueSize = 128;
    maxReconnectDelay = 30;
  };

  # Tracing options for node
  TraceOptions = {
    # The default tracer configuration
    "" = {
      backends = [
        # None, any combination, or all of the following backends can be
        # enabled, where `EKGBackend` forwards EKG resource status to
        # cardano-tracer, `Forwarder` forwards message traces and
        # `PrometheusSimple` serves cardano-node metrics directly from
        # cardano-node and defaults to use of the same port as in the legacy
        # tracing system.
        "EKGBackend"
        "Forwarder"
        "PrometheusSimple suffix 127.0.0.1 12798"

        # Only one of the following can be enabled, which determines for format
        # of node logging to stdout.
        "Stdout HumanFormatColoured"
        # "Stdout HumanFormatUncoloured"
        # "Stdout MachineFormat"
      ];

      # Each tracer can specify the level of details for printing messages.
      # Options include `DMinimal`, `DNormal`, `DDetailed`, and `DMaximum`. If
      # no implementation is given, `DNormal` is chosen.
      detail = "DNormal";

      # The severity levels, ranging from the least severe (`Debug`) to the
      # most severe (`Emergency`), provide a framework for ignoring messages
      # with severity levels below a globally configured severity cutoff.
      #
      # The full list of severities are:
      # `Debug`, `Info`, `Notice`, `Warning`, `Error`, `Critical`, `Alert` and
      # `Emergency`.
      #
      # To enhance severity filtering, there is also the option of `Silence`
      # which allows for the unconditional silencing of a specific trace,
      # essentially representing the deactivation of tracers -- a semantic
      # continuation of the functionality in the legacy system.
      severity = "Notice";
    };

    # The following tracer configurations are configured to closely match the
    # default logging seen in the legacy cardano-node tracing system.
    "BlockFetch.Decision".severity = "Silence";
    "ChainDB.AddBlockEvent.AddBlockValidation".severity = "Silence";
    "ChainDB".severity = "Info";
    "ChainSync.Client".severity = "Warning";
    "Forge.Loop".severity = "Info";
    "Forge.StateInfo".severity = "Info";
    "Mempool".severity = "Info";
    "Net.ConnectionManager.Remote.ConnectionManagerCounters".severity = "Silence";
    "Net.ConnectionManager.Remote".severity = "Info";
    "Net.ErrorPolicy.Local".severity = "Info";
    "Net.ErrorPolicy".severity = "Info";
    "Net.InboundGovernor.Remote".severity = "Info";
    "Net.InboundGovernor".severity = "Warning";
    "Net.Mux.Remote".severity = "Info";
    "Net.PeerSelection".severity = "Silence";
    "Net.Subscription.DNS".severity = "Info";
    "Net.Subscription.IP".severity = "Info";
    "Resources".severity = "Silence";
    "Startup.DiffusionInit".severity = "Info";

    # A frequency limit for the number of messages per second may also be
    # provided for any tracer.
    "BlockFetch.Client.CompletedBlockFetch".maxFrequency = 2.0;
    "ChainDB.AddBlockEvent.AddBlockValidation.ValidCandidate".maxFrequency = 2.0;
    "ChainDB.AddBlockEvent.AddedBlockToQueue".maxFrequency = 2.0;
    "ChainDB.AddBlockEvent.AddedBlockToVolatileDB".maxFrequency = 2.0;
    "ChainDB.CopyToImmutableDBEvent.CopiedBlockToImmutableDB".maxFrequency = 2.0;

    # The following messages are UTxO-HD specific. Silencing these tracers aims
    # at having comparable log line rates in messages per second on both the
    # UTxO-HD and earlier non-UTxO-HD nodes.  The additional high granularity
    # mempool silences are not redundant in the case that the top level Mempool
    # severity is switched away from silence.
    "ChainDB.LedgerEvent.Forker".severity = "Silence";
    "Mempool.AttemptAdd".severity = "Silence";
    "Mempool.LedgerFound".severity = "Silence";
    "Mempool.LedgerNotFound".severity = "Silence";
    "Mempool.SyncNotNeeded".severity = "Silence";

    # Enable this to investigate transaction validation errors.
    # "Mempool.RejectedTx".detail = "DDetailed";
  };

  # Required by the legacy tracing system, this key is still required for
  # cardano-node to start.
  minSeverity = "Critical";

  # Required by some legacy tests which may otherwise fail to start.
  defaultBackends = [];
  defaultScribes = [];
  options = {};
  setupBackends = [];
  setupScribes = [];
}
