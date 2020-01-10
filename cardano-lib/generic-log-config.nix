{
  # global filter; messages must have at least this severity to pass:
  minSeverity = "Debug";

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
    "AggregationBK"
    "KatipBK"
    "EKGViewBK"
  ];

  # if not indicated otherwise, then messages are passed to these backends:
  defaultBackends = [
    "KatipBK"
  ];

  # if wanted, the GUI is listening on this port:
  # hasGUI: 12787

  # if wanted, the EKG interface is listening on this port:
  hasEKG = 12788;
  hasPrometheus = [ "localhost" 12798 ];

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

  # more options which can be passed as key-value pairs:
  options = {
    cfokey = {
      value = "Release-1.0.0";
    };
    mapSubtrace = {
      benchmark = {
        subtrace = "ObservableTrace";
        contents = [
          "GhcRtsStats"
          "MonotonicClock"
        ];
      };
      "#ekgview" = {
        subtrace = "FilterTrace";
        contents = [
          [{ tag = "Contains";   contents = "cardano.epoch-validation.benchmark";}
           [{tag = "Contains";   contents = ".monoclock.basic.";}]
          ]
          [{ tag = "Contains";   contents = "cardano.epoch-validation.benchmark";}
           [{tag = "Contains";   contents = "diff.RTS.cpuNs.timed.";}]
          ]
          [{ tag = "StartsWith"; contents = "#ekgview.#aggregation.cardano.epoch-validation.benchmark";}
           [{tag = "Contains";   contents = "diff.RTS.gcNum.timed.";}]
          ]
        ];
      };
      "cardano.epoch-validation.utxo-stats" = {
         # Change the `subtrace` value to `Neutral` in order to log
         # `UTxO`-related messages during epoch validation.
         subtrace = "NoTrace";
      };
      "#messagecounters.aggregation" = {subtrace = "NoTrace";};
      "#messagecounters.ekgview"     = {subtrace = "NoTrace";};
      "#messagecounters.switchboard" = {subtrace = "NoTrace";};
      "#messagecounters.katip"       = {subtrace = "NoTrace";};
      "#messagecounters.monitoring"  = {subtrace = "NoTrace";};
      "cardano.node-metrics"         = {subtrace = "NoTrace";};
    };
    mapBackends = {
      "cardano.node.metrics.ChainDB"          = ["EKGViewBK"
                                                 {kind = "UserDefinedBK"; name = "LiveViewBackend";}];
      "cardano.node.BlockFetchDecision"       = [{kind = "UserDefinedBK"; name = "LiveViewBackend";}];
      "cardano.node.peers.BlockFetchDecision" = [{kind = "UserDefinedBK"; name = "LiveViewBackend";}];
    };
  };
}
