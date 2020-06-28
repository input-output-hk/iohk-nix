{lib, writeText, runCommand, jq, rust-packages, fetchurl }:
let
  calculatedVersions = ( __fromJSON (__readFile ./versions.json) ).versions;

  versions =  calculatedVersions // {
    master = {
      name = "jormungandr-master";
      version = "master";
      rev = "06f95aa06b4d2bac69314145b2882821f06e7673";
      sha256 = "034zpw3lnwrv7wz2xg460wnq3ddbylmcwk19g48zklbpl6km15fz";
      cargoSha256 = "0i1n5p3k5n9rhm62bbgxhi1q1x7nw5vbz868jfncay1mlindjlzv";
    };

    release = calculatedVersions.v0_8_9;
  };

  inherit (rust-packages.pkgs) makeJormungandr makeJcli makeJormungandr-debug makeJcli-debug;

  packages = builtins.mapAttrs (name: value: {
    jormungandr = makeJormungandr value;
    jormungandr-debug = makeJormungandr-debug value;
    jcli = makeJcli value;
    jcli-debug = makeJcli-debug value;
  }) versions;

  mkConfig = environment: let
    envVersion = environment.packages.jormungandr.version;
    defaultLogConfig = {
      level = "info";
      format = "plain";
      output = "stderr";
    };
    versionNewer071 = builtins.compareVersions envVersion "0.7.1" >= 0;
    isMaster = envVersion == "master";
  in {
      log = if (isMaster || versionNewer071)
        then [defaultLogConfig]
        else defaultLogConfig;
      rest = {
        listen = "127.0.0.1:3100";
      };
      p2p = {
        trusted_peers = environment.trustedPeers;
        topics_of_interest = {
          messages = "low";
          blocks = "normal";
        };
      };
    };

  mkConfigHydra = environment: runCommand "jormungandr-config" {
      buildInputs = [ environment.packages.jormungandr ];
    } ''
    mkdir -p $out/nix-support
    ${jq}/bin/jq . < ${__toFile "jormungandr-config.yaml" (__toJSON (mkConfig environment))} > $out/config.yaml
    ${jq}/bin/jq . < ${environment.genesisFile} > $out/genesis.yaml
    echo "${environment.genesisHash}" > $out/genesis-hash.txt
    echo "file binary-dist $out/config.yaml" > $out/nix-support/hydra-build-products
    echo "file binary-dist $out/genesis-hash.txt" >> $out/nix-support/hydra-build-products
    echo "file binary-dist $out/genesis.yaml" >> $out/nix-support/hydra-build-products
    echo "file binary-dist $out/jormungandr-version.txt" >> $out/nix-support/hydra-build-products
    jormungandr --full-version > $out/jormungandr-version.txt
  '';

  linkTo = rev:
    if __stringLength rev == 40 then
      "https://github.com/input-output-hk/jormungandr/commits/${rev}"
    else
      "https://github.com/input-output-hk/jormungandr/releases/tag/${rev}";

  configHtml = environments:
    ''
    <!DOCTYPE html>
    <html>
      <head>
        <title>Jörmungandr Status</title>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">
        <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bulma@0.8.0/css/bulma.min.css">
        <script defer src="https://use.fontawesome.com/releases/v5.3.1/js/all.js"></script>
      </head>
      <body>
        <section class="hero is-small is-primary">
          <div class="hero-body">
            <div class="container">
              <h1 class="title is-1">
                Jörmungandr
              </h1>
              <h2 class="subtitle is-3">
                Configurations
              </h2>
            </div>
          </div>
        </section>

        <section class="section">
          <div class="container">
            <div class="table-container">
              <table class="table is-narrow is-fullwidth">
                <thead>
                  <tr>
                    <th>Cluster</th>
                    <th>Version</th>
                    <th>Genesis Hash</th>
                    <th>Config</th>
                  </tr>
                </thead>
                <tbody>
                  ${toString (lib.mapAttrsToList (name: value:
                    ''
                    <tr>
                      <td>${name}</td>
                      <td><a href="${linkTo value.packages.jcli.src.rev}">${value.packages.jcli.src.rev}</a></td>
                      <td style="font-family: monospace;">${value.genesisHash}</td>
                      <td>
                        <div class="buttons has-addons">
                          <a class="button is-primary" href="${name}-config.yaml">config</a>
                          <a class="button is-info" href="${name}-genesis.yaml">genesis</a>
                        </div>
                      </td>
                    </tr>
                    ''
                  ) environments) }
                </tbody>
              </table>
            </div>
          </div>
        </section>
      </body>
    </html>
  '';

  mkConfigHtml = environments: runCommand "jormungandr-html" { buildInputs = [ jq ]; } ''
    mkdir -p $out/nix-support
    cp ${writeText "config.html" (configHtml environments)} $out/index.html
    ${
      toString (lib.mapAttrsToList (name: value:
        ''
          ${jq}/bin/jq . < ${__toFile "${name}-config.yaml" (__toJSON (mkConfig value))} > $out/${name}-config.yaml
          ${jq}/bin/jq . < ${value.genesisFile} > $out/${name}-genesis.yaml
        ''
      ) environments )
    }
    echo "report jormungandr $out index.html" > $out/nix-support/hydra-build-products
  '';

  environments = {
    itn_rewards_v1 = {
      packages = packages.v0_9_1;
      genesisHash = "8e4d2a343f3dcf9330ad9035b3e8d168e6728904262f2c434a4f8f934ec7b676";
      genesisFile = ./genesis-itn_rewards_v1.yaml;
      registryUrl = "https://github.com/cardano-foundation/incentivized-testnet-stake-poolregistry/archive/master.zip";
      syncTolerance = "600s";
      genesisYaml = fetchurl {
        url = "https://update-jormungandr-incentivized.iohk.io/genesis.yaml";
        sha256 = "0188291c74ee88b1ee8b3d8c7cd93dabf88a1a098a7c94c45adc88ba56a791f6";
      };
      block0bin = fetchurl {
        url = "https://update-jormungandr-incentivized.iohk.io/block-0.bin";
        sha256 = "5cac56e9017dd033ecc650f63262c3c47e6272e4a0e4533b9f7f6f3ddab4838e";
      };
      trustedPeers = [
        {
          address = "/ip4/13.56.0.226/tcp/3000";
        }
        {
          address = "/ip4/54.183.149.167/tcp/3000";
        }
        {
          address = "/ip4/52.9.77.197/tcp/3000";
        }
        {
          address = "/ip4/18.177.78.96/tcp/3000";
        }
        {
          address = "/ip4/3.115.154.161/tcp/3000";
        }
        {
          address = "/ip4/18.182.115.51/tcp/3000";
        }
        {
          address = "/ip4/18.184.35.137/tcp/3000";
        }
        {
          address = "/ip4/3.125.31.84/tcp/3000";
        }
        {
          address = "/ip4/3.125.183.71/tcp/3000";
        }
      ];
      daedalusPeers = [
        {
          address = "/ip4/172.31.46.19/tcp/1914";
        }
        {
          address = "/ip4/172.31.46.19/tcp/1915";
        }
        {
          address = "/ip4/52.9.132.248/tcp/3000";
        }
        {
          address = "/ip4/52.8.15.52/tcp/3000";
        }
        {
          address = "/ip4/52.53.96.239/tcp/3000";
        }
        {
          address = "/ip4/18.144.131.59/tcp/3000";
        }
        {
          address = "/ip4/13.52.165.138/tcp/3000";
        }
        {
          address = "/ip4/18.144.107.207/tcp/3000";
        }
        {
          address = "/ip4/13.114.196.228/tcp/3000";
        }
        {
          address = "/ip4/13.112.181.42/tcp/3000";
        }
        {
          address = "/ip4/54.65.218.102/tcp/3000";
        }
        {
          address = "/ip4/18.177.126.82/tcp/3000";
        }
        {
          address = "/ip4/18.178.219.241/tcp/3000";
        }
        {
          address = "/ip4/3.113.238.195/tcp/3000";
        }
        {
          address = "/ip4/3.125.75.156/tcp/3000";
        }
        {
          address = "/ip4/52.28.91.178/tcp/3000";
        }
        {
          address = "/ip4/3.124.116.145/tcp/3000";
        }
        {
          address = "/ip4/3.126.150.188/tcp/3000";
        }
        {
          address = "/ip4/3.126.223.169/tcp/3000";
        }
        {
          address = "/ip4/3.126.223.253/tcp/3000";
        }
        {
          address = "/ip4/52.206.55.62/tcp/3000";
        }
        {
          address = "/ip4/52.206.218.41/tcp/3000";
        }
        {
          address = "/ip4/52.207.170.168/tcp/3000";
        }
        {
          address = "/ip4/52.205.143.170/tcp/3000";
        }
        {
          address = "/ip4/23.20.89.92/tcp/3000";
        }
        {
          address = "/ip4/3.223.128.131/tcp/3000";
        }
        {
          address = "/ip4/13.250.150.213/tcp/3000";
        }
        {
          address = "/ip4/18.140.83.189/tcp/3000";
        }
        {
          address = "/ip4/52.77.85.27/tcp/3000";
        }
        {
          address = "/ip4/18.139.40.249/tcp/3000";
        }
        {
          address = "/ip4/18.136.70.91/tcp/3000";
        }
        {
          address = "/ip4/52.221.148.77/tcp/3000";
        }
      ];
    };

    legacy = {
      packages = packages.v0_8_0;
      genesisHash = "e03547a7effaf05021b40dd762d5c4cf944b991144f1ad507ef792ae54603197";
      genesisFile = ./genesis-legacy.yaml;
      registryUrl = "https://explorer.legacy.jormungandr-testnet.iohkdev.io/stakepool-registry/registry.zip";
      syncTolerance = "600s";
      trustedPeers = [
        {
          address = "/ip4/52.53.93.103/tcp/3000";
          id = "870a99ebe6256027826126ee75154c3bf61e934dc41ed703";
        }
        {
          address = "/ip4/13.52.188.244/tcp/3000";
          id = "d3d1dded6debbeded6bd3868274372123224a9fd85bf4ae3";
        }
        {
          address = "/ip4/54.177.223.67/tcp/3000";
          id = "784cc7f338360cf7ae4b4dedb2bda4e14c0838f32bfc239a";
        }
        {
          address = "/ip4/18.177.133.253/tcp/3000";
          id = "617956ec4fb8fb32fd8a39905c00975bef2200224584ad18";
        }
        {
          address = "/ip4/3.115.248.77/tcp/3000";
          id = "10619985ed6a15ebc69709e93f51012fbdc02c3d40e20893";
        }
        {
          address = "/ip4/3.122.157.137/tcp/3000";
          id = "97c5d5ead8b5b3ebf20801c628e580f943369b95d5a6d08a";
        }
        {
          address = "/ip4/3.123.220.150/tcp/3000";
          id = "b84f32e051f2ba6aecc64aa84909ef3307949c8ee7bd2cf3";
        }
      ];
    };

    beta = {
      packages = packages.v0_7_0;
      genesisHash = "27668e95121566df0bb2e2c11c5fd95dfe59efd570f8f592235ecff167ca3f29";
      genesisFile = ./genesis-beta.yaml;
      registryUrl = "https://explorer.beta.jormungandr-testnet.iohkdev.io/stakepool-registry/registry.zip";
      syncTolerance = "300s";
      trustedPeers = [
        {
          address = "/ip4/52.9.85.113/tcp/3000";
          id = "7f47c880339670ad98d38ad3b379e1f7853479f8ef4f6fc7";
        }
        {
          address = "/ip4/13.57.72.175/tcp/3000";
          id = "b8b20f58b34dd7a485c8cff0d67f800149b1ff220b826632";
        }
        {
          address = "/ip4/52.8.62.219/tcp/3000";
          id = "f51aa0ce82b7f061e12762bd22b84424129f690655441b8e";
        }
        {
          address = "/ip4/52.194.124.233/tcp/3000";
          id = "255df5de725cd9d1087b8a3e4ff66d65572c36ceed791679";
        }
        {
          address = "/ip4/52.197.220.18/tcp/3000";
          id = "50768a0bb41781baa551cd96fb46a62e666e97874bca1cf5";
        }
        {
          address = "/ip4/3.125.20.154/tcp/3000";
          id = "ddfea960bc2fe1aa45af9b385b6bd3e949c050df61b5b451";
        }
        {
          address = "/ip4/3.124.255.35/tcp/3000";
          id = "2b7216b51b890ef1e8ade8e513dd6f2b35173e46b08ac1a9";
        }
      ];
    };

    nightly = {
      packages = packages.v0_9_1;
      genesisHash = "9409af111b04896c756c1cee3b7f9bae8b9ed1843c9e0a5f07d92ab9b62f6f78";
      genesisFile = ./genesis-nightly.yaml;
      registryUrl = "https://github.com/input-output-hk/testnet-stakepool-registry/archive/master.zip";
      syncTolerance = "300s";
      trustedPeers = [
        {
          address = "/ip4/13.230.137.72/tcp/3000";
        }
        {
          address = "/ip4/13.230.48.191/tcp/3000";
        }
        {
          address = "/ip4/18.196.168.220/tcp/3000";
        }
        {
          address = "/ip4/3.124.132.123/tcp/3000";
        }
        {
          address = "/ip4/18.184.181.30/tcp/3000";
        }
        {
          address = "/ip4/184.169.162.15/tcp/3000";
        }
        {
          address = "/ip4/52.52.67.33/tcp/3000";
        }
      ];
    };

    qa = {
      packages = packages.v0_9_1;
      genesisHash = "b5ada4577e10513240a5457490bd855c350b378fcd762d4354b443ba49b42dda";
      genesisFile = ./genesis-qa.yaml;
      registryUrl = "https://explorer.qa.jormungandr-testnet.iohkdev.io/stakepool-registry/registry.zip";
      syncTolerance = "300s";
      trustedPeers = [
        {
          address = "/ip4/54.193.75.55/tcp/3000";
        }
        {
          address = "/ip4/13.57.122.88/tcp/3000";
        }
        {
          address = "/ip4/13.52.228.233/tcp/3000";
        }
        {
          address = "/ip4/3.115.220.90/tcp/3000";
        }
        {
          address = "/ip4/52.198.219.29/tcp/3000";
        }
        {
          address = "/ip4/3.124.136.155/tcp/3000";
        }
        {
          address = "/ip4/3.120.96.219/tcp/3000";
        }
      ];
    };
  };

  forEnvironments = f: lib.mapAttrs
    (name: env: f (env // { inherit name; }))
    environments;

in {
  inherit environments forEnvironments mkConfig mkConfigHydra versions packages
          mkConfigHtml makeJormungandr makeJcli;
}
