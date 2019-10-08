{
  extras = hackage:
    {
      packages = {
        "base58-bytestring" = (((hackage.base58-bytestring)."0.1.0").revisions).default;
        "hedgehog" = (((hackage.hedgehog)."1.0").revisions).default;
        "micro-recursion-schemes" = (((hackage.micro-recursion-schemes)."5.0.2.2").revisions).default;
        "streaming-binary" = (((hackage.streaming-binary)."0.3.0.1").revisions).default;
        iohk-skeleton = ./iohk-skeleton.nix;
        cardano-prelude = ./cardano-prelude.nix;
        cardano-prelude-test = ./cardano-prelude-test.nix;
        cardano-binary = ./cardano-binary.nix;
        cardano-binary-test = ./cardano-binary-test.nix;
        contra-tracer = ./contra-tracer.nix;
        iohk-monitoring = ./iohk-monitoring.nix;
        cborg = ./cborg.nix;
        cardano-crypto = ./cardano-crypto.nix;
        canonical-json = ./canonical-json.nix;
        };
      compiler.version = "8.6.5";
      compiler.nix-name = "ghc865";
      };
  resolver = "lts-13.26";
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "iohk-monitoring" = {
            flags = {
              "disable-examples" = lib.mkOverride 900 true;
              "disable-ekg" = lib.mkOverride 900 true;
              "disable-systemd" = lib.mkOverride 900 true;
              "disable-prometheus" = lib.mkOverride 900 true;
              "disable-gui" = lib.mkOverride 900 true;
              "disable-graylog" = lib.mkOverride 900 true;
              };
            };
          };
        })
    { packages = {}; }
    {
      packages = {
        "iohk-monitoring" = {
          flags = {
            "disable-examples" = true;
            "disable-ekg" = true;
            "disable-systemd" = true;
            "disable-prometheus" = true;
            "disable-gui" = true;
            "disable-graylog" = true;
            };
          };
        };
      }
    ];
  compiler = "ghc-8.6.5";
  }