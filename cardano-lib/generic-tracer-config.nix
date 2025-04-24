{
  # Cardano-tracer settings here do *not* affect the message detail or
  # filtering of what the provider source is configured to emit.
  #
  # Cardano-tracer will use default values where parameters are set to null.

  # ------------

  # WarnRTViewMissing indicates whether to provide a warning if RTView is
  # requested in config but cardano-tracer was built without it.
  #
  # If null cardano-tracer will set a default: true if RTView
  # config is provided but cardano-tracer was built without it, false
  # otherwise.
  WarnRTViewMissing = null;

  # ekgRequestFreq specifies the period of how often EKG metrics will be
  # requested, in seconds. For example, if ekgRequestFreq is 10, cardano-tracer
  # will ask for new EKG metrics every ten seconds. There is no limit as
  # loRequestNum, so every request returns all the metrics the provider has at
  # that moment of time.
  #
  # If null cardano-tracer will set a default: 1.
  ekgRequestFreq = null;

  # When receiving forwarded metrics, cardano-tracer can request a full
  # set of EKG metrics, or a delta from the previous request.
  #
  # If ekgRequestFull is set true, a full set of metrics will be sent on each
  # request. Otherwise, on false, only a metrics delta to the previous request
  # will be sent.
  #
  # If null cardano-tracer will set a default: false.
  ekgRequestFull = null;

  # Whether to enable an EKG http interface for process monitoring.
  hasEKG = {
    # The host to bind if EKG is enabled.
    epHost = "127.0.0.1";

    # The port to listen on if EKG is enabled.
    epPort = 12788;
  };

  # Whether to enable a prometheus export of metrics.
  hasPrometheus = {
    # The host to bind if prometheus is enabled.
    epHost = "127.0.0.1";

    # The port to listen on if prometheus is enabled.
    #
    # Defaults to the legacy prometheus listening port, 12798, plus 10.
    # This avoids a conflict with the cardano-node default metrics port binding
    # when PrometheusSimple backend is in use.
    epPort = 12808;
  };

  # loRequestNum specifies the number of log items that will be requested from
  # the providing source. For example, if loRequestNum is 10, cardano-tracer
  # will periodically ask for 10 log items in one request. This value is useful
  # for fine-tuning network traffic: it is possible to ask 50 log items in one
  # request, or ask for them in 50 requests one at a time. loRequestNum is the
  # maximum number of log items. For example, if cardano-tracer requests 50 log
  # items but the provider has only 40 at that moment, these 40 items will be
  # returned and the request won't block to wait for an additional 10 items.
  #
  # If null cardano-tracer will set a default: 100.
  loRequestNum = null;

  # Configure default logging
  logging = [
    {
      # logFormat specifies the format of logs. There are two possible modes:
      # `ForMachine` and `ForHuman`. ForMachine is for JSON format, ForHuman is
      # for human-friendly text format. Since the logging option accepts a
      # list, more than one logging section can be declared.
      logFormat = "ForHuman";

      # logMode specifies logging mode. There are two possible modes:
      # `FileMode` and `JournalMode`. FileMode is for storing logs to the
      # files, JournalMode is for storing them in systemd's journal. If you
      # choose JournalMode, the logRoot option, will be ignored.
      logMode = "FileMode";

      # logRoot specifies the path to the root directory. This directory will
      # contain all the subdirectories with the log files inside. Remember that
      # each subdirectory corresponds to the particular provider. If the root
      # directory does not exist, it will be created.
      logRoot = "/tmp/cardano-tracer";
    }
  ];

  # Passing metrics help annotations to cardano-tracer can be done as an
  # attribute set of strings from metric's name to help text where
  # cardano-tracer's internal metric names have to be used as the attribute
  # names.
  #
  # If such a set is already available as JSON in a file, this option can
  # be declared as a string of the path to such a file.
  #
  # Any metrics prefix declared in provider config, such as
  # `TraceOptionMetricsPrefix` in cardano-node, should not be included in the
  # attribute name. Similarly, metric type suffixes such as `.int` or `.real`,
  # should also not be included.
  #
  # The effect of this option applies to prometheus metrics only, ie: not
  # EKG.
  #
  # An example of usage is for node provided metrics is:
  #
  # metricsHelp = {
  #   "Mem.resident" = "Kernel-reported RSS (resident set size)";
  #   "RTS.gcMajorNum" = "Major GCs";
  # };
  metricsHelp = null;

  # If metricsNoSuffix is set true, metrics name suffixes, like "_int", will be
  # dropped, thus increasing the similarity with legacy tracing system names.
  #
  # The effect of this option applies to prometheus metrics only, ie: not EKG.
  #
  # If null cardano-tracer will set a default: false.
  metricsNoSuffix = null;

  # Configure default cardano-tracer operational mode
  network = {
    # The tag must be either `AcceptAt` or `ConnectTo`.
    #
    # AcceptAt means that cardano-tracer works as a server: it receives network
    # connections from providers such as node via a single local socket
    # provided by cardano-tracer.
    #
    # ConnectTo means that cardano-tracer works as a client: it establishes
    # network connections with local socket(s) provided by the provider(s).
    # In this case a socket is used for each provider.
    #
    # Except for special use cases, AcceptAt is the recommended tag as it
    # supports dynamic provider addition or removal without requiring
    # cardano-tracer reconfiguration and restart.
    tag = "AcceptAt";

    # If the network tag is AcceptAt, contents expects a string declaring the
    # socket path that cardano-tracer will create and listen on.
    contents = "/tmp/cardano-tracer.socket";

    # On the other hand, if network tag is ConnectTo, contents expects a list
    # of strings declaring the socket paths created by provider(s) which
    # cardano-tracer will connect to.
    # contents = [
    #   "/tmp/cardano-node-1.sock"
    #   "/tmp/cardano-node-1.sock"
    #   "/tmp/cardano-node-1.sock"
    # ];
  };

  # The network magic of the cardano environment which will be connected with
  # cardano-tracer.  The per environment `tracerConfig` attribute provided by
  # cardano-lib will merge each environment's network magic with this
  # generic-tracer-config.
  # networkMagic = ... ;

  # The period for tracing cardano-tracer's own resource usage in milliseconds.
  # For example, if set to 60000, resources will traced every 60 seconds. If null
  # cardano-tracer will not display resource usage.
  resourceFreq = null;

  # Configure default rotation.
  #
  # Please note that if the rotation declaration is skipped, all log items will
  # be stored in a single file, and usually that's not what is desired.
  #
  # This option will be ignored if all logging has `logMode` configured
  # as `JournalMode`.
  rotation = {

    # rpFrequencySecs specifies rotation period, in seconds.
    rpFrequencySecs = 60;

    # rpKeepFilesNum specifies the number of the log files that will be kept.
    # The last (newest) log files will always be the ones kept, whereas the first
    # (oldest) log files will be purged.
    rpKeepFilesNum = 14;

    # rpLogLimitBytes specifies the maximum size of the log file, in bytes.
    # Once the size of the current log file reaches this value, a new log file
    # will be created.
    rpLogLimitBytes = 10000000;

    # rpMaxAgeHours specifies the lifetime of the log file in hours. Once the
    # log file reaches this value, it is treated as outdated and will be deleted.
    # Please note that N last log files, specified by option rpKeepFilesNum, will
    # be kept even if they are outdated. If the rpMaxAgeMinutes option is also
    # declared then it takes precedence.
    rpMaxAgeHours = 24;

    # rpMaxAgeMinutes specifies the lifetime of the log file in minutes. Once
    # the log file reaches this value, it is treated as outdated and will be
    # deleted.  Please note that N last log files, specified by option
    # rpKeepFilesNum, will be kept even if they are outdated. If the
    # rpMaxAgeHours option is also declared this option takes precedence.
    # rpMaxAgeMinutes = 24 * 60;
  };

  # verbosity specifies the level for cardano-tracer itself.  There are 3
  # levels:
  #
  # Minimum - cardano-tracer will work as silently as possible.
  # ErrorsOnly - messages about problems will be shown in standard output.
  # Maximum - all the messages will be shown in standard output. Caution: the number of messages can be huge.
  #
  # If null cardano-tracer will set a default: ErrorsOnly.
  verbosity = null;
}
