{lib, writeText, runCommand, jq, rust-packages }:
let
  versions = rec {
    release = v0_7_0;

    v0_7_0 = {
      version = "0.7.0";
      sha256 = "0hhbn383z3j06llx887qpx7gmxyy7r1n2m79kx0hshhyd90w7rcs";
      cargoSha256 = "0fqpm0a1824dirb3f5d4yw7vb8xrpj03n6gxw7rlfjbhy025spqh";
    };

    v0_7_1 = {
      version = "0.7.1";
      sha256 = "1qc8wrmddggfw1b4qyv03z7zimnkjl0qi91zaz53w2ghmfqkli25";
      cargoSha256 = "14v7v9rl04yajwxh7qcpzjnc3swmpdbmjqw7arnms8cbdfbqc9q6";
    };

    v0_7_2 = {
      version = "0.7.2";
      sha256 = "0h75yf7ma4vj0c276qn12pb9rmwgqgx29k4nf828vaackzpwpby7";
      cargoSha256 = "14v7v9rl04yajwxh7qcpzjnc3swmpdbmjqw7arnms8cbdfbqc9q6";
    };

    v0_7_3 = {
      version = "0.7.3";
      sha256 = "1k71lnxvxsnkr8z7qf7sgpmzmm4ck1bhxswri374jch45b5dc09m";
      cargoSha256 = "14v7v9rl04yajwxh7qcpzjnc3swmpdbmjqw7arnms8cbdfbqc9q6";
    };

    v0_7_4 = {
      version = "0.7.4";
      sha256 = "0ih99p0rx65pvdn9hvv5y8p46h9m1zs8f7wwh23w8zz0r7jicbmf";
      cargoSha256 = "14v7v9rl04yajwxh7qcpzjnc3swmpdbmjqw7arnms8cbdfbqc9q6";
    };

    v0_7_5 = {
      version = "0.7.5";
      sha256 = "0x1f046z5nkp1q0pdc3xfrvnc7rxswjch16xrw4rdi5kkd6p65bj";
      cargoSha256 = "14v7v9rl04yajwxh7qcpzjnc3swmpdbmjqw7arnms8cbdfbqc9q6";
    };

    v0_8_0-rc1 = {
      version = "0.8.0-rc1";
      sha256 = "1ingf3hp35b762l0n22yaicyyrql6sqsn0qs11wbb3qifa2s1lvb";
      cargoSha256 = "14v7v9rl04yajwxh7qcpzjnc3swmpdbmjqw7arnms8cbdfbqc9q6";
    };

    v0_8_0-rc2 = {
      version = "0.8.0-rc2";
      sha256 = "1b8vkh5yzql39a2lkzf72v3pzshwf3kx9xqr1ka2p58hr5a82dl1";
      cargoSha256 = "14v7v9rl04yajwxh7qcpzjnc3swmpdbmjqw7arnms8cbdfbqc9q6";
    };

    v0_8_0-rc3 = {
      version = "0.8.0-rc3";
      sha256 = "0csqd60hz9vixvyp1rracz18lvjkspsm9r0df7sxs5inp1f3x8mk";
      cargoSha256 = "14v7v9rl04yajwxh7qcpzjnc3swmpdbmjqw7arnms8cbdfbqc9q6";
    };

    v0_8_0-rc4 = {
      version = "0.8.0-rc4";
      sha256 = "0jm4mflmjmxc3p25js3n80w1x744bh5nb1c13bd25hk88xhyz2dn";
      cargoSha256 = "14v7v9rl04yajwxh7qcpzjnc3swmpdbmjqw7arnms8cbdfbqc9q6";
    };

    master = {
      name = "jormungandr-master";
      version = "master";
      rev = "1924085a09c285954b992af44706fbe82da02d64";
      sha256 = "1dap67c194r0n9i4lfin5nlhgzvhq0lx25a6w6nspc71chr8x0w7";
      cargoSha256 = "0fqpm0a1824dirb3f5d4yw7vb8xrpj03n6gxw7rlfjbhy025spqh";
    };
  };

  inherit (rust-packages.pkgs) makeJormungandr makeJcli;

  packages = builtins.mapAttrs (name: value:
    { jormungandr = makeJormungandr value; jcli = makeJcli value; }
  ) versions;

  mkConfig = environment:
    let
    in {
      log = [{
        level = "info";
        format = "plain";
        output = "stderr";
      }];
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

  environments = {
    itn_balance_check = {
      packages = packages.v0_8_0-rc1;
      genesisHash = "0f9d564199ad7f71af3daaff4b6997cb7f2e3d7c422fa29097f5d6a018c440d1";
      genesisFile = ./genesis-mock.yaml;
      syncTolerance = "600s";
      trustedPeers = [
        {
         address = "/ip4/54.183.206.246/tcp/3000";
         id = "67282e0bf1dcdf5e4559c85b057de122ba7a248f16ac5226";
        }
        {
          address = "/ip4/52.8.126.235/tcp/3000";
          id = "75534bbbb7dddfeee3783ae5e7983c28349209138351c278";
        }
        {
          address = "/ip4/3.113.195.95/tcp/3000";
          id = "617ff3920bac9e90faec3adc5d32c2937cbb6b8528b1b22d";
        }
        {
          address = "/ip4/18.176.14.159/tcp/3000";
          id = "55f82104122f4d5101ae549935943071c8c646f1bd62cd0b";
        }
        {
          address = "/ip4/3.124.166.70/tcp/3000";
          id = "96a84a81a74715c7fb29d906ddff32b2874aac4d14acc7db";
        }
        {
          address = "/ip4/3.125.15.153/tcp/3000";
          id = "97fd215d7e294b5943960056af1e1a0316c3593a4aa3cb76";
        }
        {
          address = "/ip4/3.124.103.156/tcp/3000";
          id = "173ca3b1198816989c585785a8c912fea3c5ec14895eaa6b";
        }
      ];
    };

    beta = {
      packages = packages.v0_7_0;
      genesisHash = "27668e95121566df0bb2e2c11c5fd95dfe59efd570f8f592235ecff167ca3f29";
      genesisFile = ./genesis-beta.yaml;
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
      packages = packages.v0_8_0-rc4;
      genesisHash = "a30d30b452ee801633dd1c71751ad07579c0f742a3a1d0fd836d0bc9caea45f6";
      genesisFile = ./genesis-nightly.yaml;
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
      packages = packages.v0_8_0-rc4;
      genesisHash = "1763fc87186dde2c2d18a19d06a4f02932d7ce1b60ea1267ae34ddab3e63716e";
      genesisFile = ./genesis-qa.yaml;
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
  inherit environments forEnvironments mkConfig mkConfigHydra versions packages;
}
