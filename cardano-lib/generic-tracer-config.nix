{
  # Cardano-tracer will use default values where parameters are set to null.
  WarnRTViewMissing = null;
  ekgRequestFreq = null;
  ekgRequestFull = null;

  # Whether to enable an EKG http interface for process monitoring.
  hasEKG = {
    epHost = "127.0.0.1";
    epPort = 12788;
  };

  # Whether to enable a prometheus export of metrics.
  hasPrometheus = {
    epHost = "127.0.0.1";
    epPort = 12808;
  };

  loRequestNum = null;

  # Configure default logging
  logging = [
    {
      logFormat = "ForHuman";
      logMode = "FileMode";
      logRoot = "/tmp/cardano-tracer";
    }
  ];

  metricsHelp = null;
  metricsNoSuffix = null;

  # Configure default cardano-tracer operational mode
  network = {
    contents = "/tmp/cardano-tracer.socket";
    tag = "AcceptAt";
  };

  # The network magic will need to be set per environment
  # networkMagic = ... ;

  resourceFreq = null;

  # Configure default rotation
  rotation = {
    rpFrequencySecs = 60;
    rpKeepFilesNum = 14;
    rpLogLimitBytes = 10000000;
    rpMaxAgeHours = 24;
  };

  verbosity = null;
}
