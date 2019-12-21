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
    mapSubtrace = {
      "#messagecounters.switchboard" = {
         subtrace = "NoTrace";
      };
      "#messagecounters.katip" = {
         subtrace = "NoTrace";
      };
      "#messagecounters.ekgview" = {
         subtrace = "NoTrace";
      };
    };
    mapBackends = {
      "cardano_byron_proxy.ChainDB.blockNum" = [
         "EKGViewBK"
      ];
    };
  };
}
