{lib, writeText, runCommand, jq}:
let
  mkConfig = environment: {
    log = {
      level = "info";
      format = "plain";
      output = "stderr";
    };
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
  mkConfigHydra = environment: runCommand "jormungandr-config" { } ''
    mkdir -p $out/nix-support
    ${jq}/bin/jq . < ${__toFile "jormungandr-config.yaml" (__toJSON (mkConfig environment))} > $out/config.yaml
    ${jq}/bin/jq . < ${environment.genesisFile} > $out/genesis.yaml
    echo "${environment.genesisHash}" > $out/genesis-hash.txt
    echo "file binary-dist $out/config.yaml" > $out/nix-support/hydra-build-products
    echo "file binary-dist $out/genesis-hash.txt" >> $out/nix-support/hydra-build-products
    echo "file binary-dist $out/genesis.yaml" >> $out/nix-support/hydra-build-products

  '';
  environments = {
    itn_balance_check = {
      genesisHash = "7be0014ffe4e5fd74aeb2916bd85cbfdaf52c2d738e4883f4cf52f6f695a6e2d";
      genesisFile = ./genesis-mock.yaml;
      syncTolerance = "300s";
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
      genesisHash = "adbdd5ede31637f6c9bad5c271eec0bc3d0cb9efb86a5b913bb55cba549d0770";
      genesisFile = ./genesis-beta.yaml;
      syncTolerance = "300s";
      trustedPeers = [
        {
          address = "/ip4/3.115.194.22/tcp/3000";
          id = "ed25519_pk1npsal4j9p9nlfs0fsmfjyga9uqk5gcslyuvxy6pexxr0j34j83rsf98wl2";
        }
        {
          address = "/ip4/13.113.10.64/tcp/3000";
          id = "ed25519_pk16pw2st5wgx4558c6temj8tzv0pqc37qqjpy53fstdyzwxaypveys3qcpfl";
        }
        {
          address = "/ip4/52.57.214.174/tcp/3000";
          id = "ed25519_pk1v4cj0edgmp8f2m5gex85jglrs2ruvu4z7xgy8fvhr0ma2lmyhtyszxtejz";
        }
        {
          address = "/ip4/3.120.96.93/tcp/3000";
          id = "ed25519_pk10gmg0zkxpuzkghxc39n3a646pdru6xc24rch987cgw7zq5pmytmszjdmvh";
        }
        {
          address = "/ip4/52.28.134.8/tcp/3000";
          id = "ed25519_pk1unu66eej6h6uxv4j4e9crfarnm6jknmtx9eknvq5vzsqpq6a9vxqr78xrw";
        }
        {
          address = "/ip4/13.52.208.132/tcp/3000";
          id = "ed25519_pk15ppd5xlg6tylamskqkxh4rzum26w9acph8gzg86w4dd9a88qpjms26g5q9";
        }
        {
          address = "/ip4/54.153.19.202/tcp/3000";
          id = "ed25519_pk1j9nj2u0amlg28k27pw24hre0vtyp3ge0xhq6h9mxwqeur48u463s0crpfk";
        }
      ];
    };
    nightly = {
      genesisHash = "cfd99bc54ebf44b44e72db7e2d48a40499888781e7628ea0fbf286bfd480ca58";
      genesisFile = ./genesis-nightly.yaml;
      syncTolerance = "300s";
      trustedPeers = [
        {
          address = "/ip4/13.230.137.72/tcp/3000";
          id = "e4fda5a674f0838b64cacf6d22bbae38594d7903aba2226f";
        }
        {
          address = "/ip4/13.230.48.191/tcp/3000";
          id = "c32e4e7b9e6541ce124a4bd7a990753df4183ed65ac59e34";
        }
        {
          address = "/ip4/18.196.168.220/tcp/3000";
          id = "74a9949645cdb06d0358da127e897cbb0a7b92a1d9db8e70";
        }
        {
          address = "/ip4/3.124.132.123/tcp/3000";
          id = "431214988b71f3da55a342977fea1f3d8cba460d031a839c";
        }
        {
          address = "/ip4/18.184.181.30/tcp/3000";
          id = "e9cf7b29019e30d01a658abd32403db85269fe907819949d";
        }
        {
          address = "/ip4/184.169.162.15/tcp/3000";
          id = "acaba9c8c4d8ca68ac8bad5fe9bd3a1ae8de13816f40697c";
        }
        {
          address = "/ip4/13.56.87.134/tcp/3000";
          id = "bcfc82c9660e28d4dcb4d1c8a390350b18d04496c2ac8474";
        }
      ];
    };
    qa = {
      genesisHash = "1fc80a7c3dcdf50fd967a266a6bba186c8e7a1f600334479e8ffaf779e4d4c8a";
      genesisFile = ./genesis-qa.yaml;
      syncTolerance = "300s";
      trustedPeers = [
        {
          address = "/ip4/54.193.75.55/tcp/3000";
          id = "91db277a8cbcbb91b93f1814c8c3a67bf410b56a1dd10fcb";
        }
        {
          address = "/ip4/13.57.122.88/tcp/3000";
          id = "6d9e2b32e9b509503d5e4194f0680d4f700a2748dcd18918";
        }
        {
          address = "/ip4/13.52.228.233/tcp/3000";
          id = "1450214c4cd6cf6f520737eeb3460d9295f2a0fadefd1b39";
        }
        {
          address = "/ip4/3.115.220.90/tcp/3000";
          id = "d1d7ada52d17c16453b0ef1d953041e42c99ae2bc52cec3f";
        }
        {
          address = "/ip4/52.198.219.29/tcp/3000";
          id = "233aac9c2aa8055bc605e4303b9acca597f2db6be942f90a";
        }
        {
          address = "/ip4/3.124.136.155/tcp/3000";
          id = "ac1a4f64dca99bd743e4e495dea360c33f2f8545ecd96c39";
        }
        {
          address = "/ip4/3.120.96.219/tcp/3000";
          id = "d7702d62ab4336626a6786616cde1988d9735cac2d4c3685";
        }
      ];
    };
  };
  forEnvironments = f: lib.mapAttrs
    (name: env: f (env // { inherit name; }))
    environments;

in {
  inherit environments forEnvironments mkConfig mkConfigHydra;
}
