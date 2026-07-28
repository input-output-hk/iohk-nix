{
  # Generic cardano-submit-api tracing configuration.

  # Set the metrics prefix name for submit-api metrics.
  # Disable this for the legacy behavior of no metrics name prefix.
  TraceOptionMetricsPrefix = "cardano.submit.api.metrics.";

  # Tracing options for submit-api
  TraceOptions = {
    # The default submit-api configuration
    "" = {
      backends = [
        # Forwarder and PrometheusSimple backends are not available for submit-api.
        "EKGBackend"

        # Only one of the following can be enabled, which determines for format
        # of submit-api logging to stdout.
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

    # Submit-api emits a small, fixed set of namespaces, all under the
    # `TxSubmitApi` root. Their built-in severities are already sensible, so
    # none are overridden by default. With the `Info` cutoff above everything
    # here is emitted except `InitializeMetrics`, which includes one line per
    # transaction submitted or failed. Raising the cutoff to `Notice` silences
    # every trace a healthy submit-api produces, leaving only warnings and
    # errors:
    #
    #   TxSubmitApi.Application.Stopping                 (Info)
    #   TxSubmitApi.Application.InitializeMetrics        (Debug; below the cutoff above)
    #   TxSubmitApi.Endpoint.ListeningOnPort             (Info)
    #   TxSubmitApi.Endpoint.Exception                   (Error)
    #   TxSubmitApi.Endpoint.SubmittedTransaction        (Info; one per submitted tx)
    #   TxSubmitApi.Endpoint.FailedToSubmitTransaction   (Info; one per failed tx)
    #   TxSubmitApi.Endpoint.Exiting                     (Info)
    #   TxSubmitApi.Metrics.Started                      (Info)
    #   TxSubmitApi.Metrics.Error                        (Warning)
    #   TxSubmitApi.Metrics.PortOccupied                 (Warning)
    #   TxSubmitApi.Metrics.PortNotBound                 (Error)
  };
}
