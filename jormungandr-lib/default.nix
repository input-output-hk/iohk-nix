{lib, writeText, runCommand, jq, rust-packages, fetchurl }:
let
  calculatedVersions = ( __fromJSON (__readFile ./versions.json) ).versions;

  versions =  calculatedVersions // {
    master = {
      name = "jormungandr-master";
      version = "master";
      rev = "cfd1006d56e0619d14e4dd575a0164e3b9897974";
      sha256 = "1r7ijb1xhcpzj2vlz3a7ips08l2drlx0n7jr3qjv2718yhjzxq9n";
      cargoSha256 = "0nsww7sjay64nwd5851dsjdknwz8cp4x003ivz0xjsw226hdb0jp";
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
      packages = packages.v0_8_15;
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
          id = "7ddf203c86a012e8863ef19d96aabba23d2445c492d86267";
        }
        {
          address = "/ip4/54.183.149.167/tcp/3000";
          id = "df02383863ae5e14fea5d51a092585da34e689a73f704613";
        }
        {
          address = "/ip4/52.9.77.197/tcp/3000";
          id = "fcdf302895236d012635052725a0cdfc2e8ee394a1935b63";
        }
        {
          address = "/ip4/18.177.78.96/tcp/3000";
          id = "fc89bff08ec4e054b4f03106f5312834abdf2fcb444610e9";
        }
        {
          address = "/ip4/3.115.154.161/tcp/3000";
          id = "35bead7d45b3b8bda5e74aa12126d871069e7617b7f4fe62";
        }
        {
          address = "/ip4/18.182.115.51/tcp/3000";
          id = "8529e334a39a5b6033b698be2040b1089d8f67e0102e2575";
        }
        {
          address = "/ip4/18.184.35.137/tcp/3000";
          id = "06aa98b0ab6589f464d08911717115ef354161f0dc727858";
        }
        {
          address = "/ip4/3.125.31.84/tcp/3000";
          id = "8f9ff09765684199b351d520defac463b1282a63d3cc99ca";
        }
        {
          address = "/ip4/3.125.183.71/tcp/3000";
          id = "9d15a9e2f1336c7acda8ced34e929f697dc24ea0910c3e67";
        }
      ];
      daedalusPeers = [
        {
          address = "/ip4/172.31.46.19/tcp/1914";
          id = "888888880000000000000000000000000000000000000001";
        }
        {
          address = "/ip4/172.31.46.19/tcp/1915";
          id = "888888880000000000000000000000000000000000000002";
        }
        {
          address = "/ip4/52.9.132.248/tcp/3000";
          id = "671a9e7a5c739532668511bea823f0f5c5557c99b813456c";
        }
        {
          address = "/ip4/52.8.15.52/tcp/3000";
          id = "18bf81a75e5b15a49b843a66f61602e14d4261fb5595b5f5";
        }
        {
          address = "/ip4/52.53.96.239/tcp/3000";
          id = "242de1b2966b81dd05a0fc8765574b1e5eb8252e97ade80f";
        }
        {
          address = "/ip4/18.144.131.59/tcp/3000";
          id = "d9965986c5309911c117252874e41680f90c95ed22bbe286";
        }
        {
          address = "/ip4/13.52.165.138/tcp/3000";
          id = "af79431d77a23da44d5e272f8967529c0b6d5c9268c436d8";
        }
        {
          address = "/ip4/18.144.107.207/tcp/3000";
          id = "9a0bb93459881d7d8904a7417b6300b87e919d1ff019c7b2";
        }
        {
          address = "/ip4/13.114.196.228/tcp/3000";
          id = "7e1020c2e2107a849a8353876d047085f475c9bc646e42e9";
        }
        {
          address = "/ip4/13.112.181.42/tcp/3000";
          id = "52762c49a84699d43c96fdfe6de18079fb2512077d6aa5bc";
        }
        {
          address = "/ip4/54.65.218.102/tcp/3000";
          id = "cea042779658095850c051e883690c3b2ff7a5d5c3d9f662";
        }
        {
          address = "/ip4/18.177.126.82/tcp/3000";
          id = "7537e6aea50290d54590b003d264886a1751fe8f5977d771";
        }
        {
          address = "/ip4/18.178.219.241/tcp/3000";
          id = "9f3292609c03eef46f94684f49529f2fc7da2939a178bbdc";
        }
        {
          address = "/ip4/3.113.238.195/tcp/3000";
          id = "1a1316168fa2f1d694dfa5e17925cc11b94b942002df7fa2";
        }
        {
          address = "/ip4/3.125.75.156/tcp/3000";
          id = "22fb117f9f72f38b21bca5c0f069766c0d4327925d967791";
        }
        {
          address = "/ip4/52.28.91.178/tcp/3000";
          id = "23b3ca09c644fe8098f64c24d75d9f79c8e058642e63a28c";
        }
        {
          address = "/ip4/3.124.116.145/tcp/3000";
          id = "99cb10f53185fbef110472d45a36082905ee12df8a049b74";
        }
        {
          address = "/ip4/3.126.150.188/tcp/3000";
          id = "e1a000dae7605d9f1d60bc71f76816647bb768dcf69bee4a";
        }
        {
          address = "/ip4/3.126.223.169/tcp/3000";
          id = "2482768d750bb53bd5c300154aeafd3681b1115c34344c41";
        }
        {
          address = "/ip4/3.126.223.253/tcp/3000";
          id = "b43a4f5ba33d72b5afaababfce659f5bc377508b34c8be39";
        }
        {
          address = "/ip4/52.206.55.62/tcp/3000";
          id = "1723626a4de177745ce08eeac07eb2e38c7f242f0c6022ff";
        }
        {
          address = "/ip4/52.206.218.41/tcp/3000";
          id = "49be46e3dbda53110d3e33739ac32fa33fc818b77b4a8e1c";
        }
        {
          address = "/ip4/52.207.170.168/tcp/3000";
          id = "744efd32219509e256a597faa4408b1bdf82b63dab356215";
        }
        {
          address = "/ip4/52.205.143.170/tcp/3000";
          id = "74c8843260a9d245ec0bc3eeb9724163b58fb8363990f5eb";
        }
        {
          address = "/ip4/23.20.89.92/tcp/3000";
          id = "89494006e5b1728a9bcc8c4aa9dd77adf8a833a043590f22";
        }
        {
          address = "/ip4/3.223.128.131/tcp/3000";
          id = "fa899ef0b821c93a230fcbc6aae166d817e562767b6822cf";
        }
        {
          address = "/ip4/13.250.150.213/tcp/3000";
          id = "0e94e5b313de8fe39e7d7280c26dbcab62f55902b2f60b73";
        }
        {
          address = "/ip4/18.140.83.189/tcp/3000";
          id = "91d3343d2148241533e95d42fbda9d2ba837c60ae8b029b9";
        }
        {
          address = "/ip4/52.77.85.27/tcp/3000";
          id = "cd6e0d70fe403e9265a60182df53ed0582e88112fb6a6282";
        }
        {
          address = "/ip4/18.139.40.249/tcp/3000";
          id = "75a84b308757207d64537e7305490b065bfb824cad8cd77c";
        }
        {
          address = "/ip4/18.136.70.91/tcp/3000";
          id = "8254c33b3d2b43999ce47e3576ecbd84463ed984d96554df";
        }
        {
          address = "/ip4/52.221.148.77/tcp/3000";
          id = "472e79dcd34e6894e939c98b55b7677c0493e387fe46237f";
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
      packages = packages.v0_8_15;
      genesisHash = "9409af111b04896c756c1cee3b7f9bae8b9ed1843c9e0a5f07d92ab9b62f6f78";
      genesisFile = ./genesis-nightly.yaml;
      registryUrl = "https://github.com/input-output-hk/testnet-stakepool-registry/archive/master.zip";
      syncTolerance = "300s";
      trustedPeers = [
        {
          address = "/ip4/13.230.137.72/tcp/3000";
          id = "fe3332044877b2034c8632a08f08ee47f3fbea6c64165b3b";
        }
        {
          address = "/ip4/13.230.48.191/tcp/3000";
          id = "c38aabb936944776ef15bbe4b5b02454c46a8a80d871f873";
        }
        {
          address = "/ip4/18.196.168.220/tcp/3000";
          id = "7e2222179e4f3622b31037ede70949d232536fdc244ca3d9";
        }
        {
          address = "/ip4/3.124.132.123/tcp/3000";
          id = "9085fa5caeb39eace748a7613438bd2a62c8c8ee00040b71";
        }
        {
          address = "/ip4/18.184.181.30/tcp/3000";
          id = "f131b71d65c49116f3c23c8f1dd7ceaa98f5962979133404";
        }
        {
          address = "/ip4/184.169.162.15/tcp/3000";
          id = "fdb88d08c7c759b5d30e854492cb96f8203c2d875f6f3e00";
        }
        {
          address = "/ip4/52.52.67.33/tcp/3000";
          id = "3d1f8891bf53eb2946a18fb46cf99309649f0163b4f71b34";
        }
      ];
    };

    qa = {
      packages = packages.v0_8_15;
      genesisHash = "b5ada4577e10513240a5457490bd855c350b378fcd762d4354b443ba49b42dda";
      genesisFile = ./genesis-qa.yaml;
      registryUrl = "https://explorer.qa.jormungandr-testnet.iohkdev.io/stakepool-registry/registry.zip";
      syncTolerance = "300s";
      trustedPeers = [
        {
          address = "/ip4/54.193.75.55/tcp/3000";
          id = "a85bfdff5bd4ea55beac7d5644e71958db6cb2c6ec600553";
        }
        {
          address = "/ip4/13.57.122.88/tcp/3000";
          id = "725ef9c3900a8bfb970d2dc403319f77b354806fa4a28158";
        }
        {
          address = "/ip4/13.52.228.233/tcp/3000";
          id = "251ed5489e85503358e6fc9b6dec8cd01180444860d83d5a";
        }
        {
          address = "/ip4/3.115.220.90/tcp/3000";
          id = "cede82e828ccd57ed5bd21c765680aa5177c3c744b2e5fb3";
        }
        {
          address = "/ip4/52.198.219.29/tcp/3000";
          id = "a9005255e416b5b6050be795c5fc89d84c1612bfd9576809";
        }
        {
          address = "/ip4/3.124.136.155/tcp/3000";
          id = "22075b4f1abaef6d42cbad8f6e9d6f8264cec1881e6c0b6f";
        }
        {
          address = "/ip4/3.120.96.219/tcp/3000";
          id = "02c3c4398064548430bad21718599d10cb457d82a1b32ff5";
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
