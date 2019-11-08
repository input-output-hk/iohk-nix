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
    beta = {
      genesisHash = "adbdd5ede31637f6c9bad5c271eec0bc3d0cb9efb86a5b913bb55cba549d0770";
      genesisFile = ./genesis-beta.yaml;
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
      trustedPeers = [
        {
          address = "/ip4/13.230.137.72/tcp/3000";
          id = "980657fef80a5d2bce23bb9d60c3266b5a2311ea5f3f6746";
        }
        {
          address = "/ip4/13.230.48.191/tcp/3000";
          id = "8477a0e060d0d1797fdaa6c7b78bc684f3fc26c926f15960";
        }
        {
          address = "/ip4/18.196.168.220/tcp/3000";
          id = "f291122fc279c29e302a0af22e91a468f858fd3e82bcf356";
        }
        {
          address = "/ip4/3.124.132.123/tcp/3000";
          id = "714b8e0d7ccbacb37aa4f49a380f75adb8b55d3e47d45189";
        }
        {
          address = "/ip4/18.184.181.30/tcp/3000";
          id = "75297c04687495e3ee6f22f11f912975aa485b3e0e5a33de";
        }
        {
          address = "/ip4/184.169.162.15/tcp/3000";
          id = "9afc6084584c924b4c7af6a518d55d03341c0c19443833fa";
        }
        {
          address = "/ip4/13.56.87.134/tcp/3000";
          id = "5dc5f6cbc13b8e305c181a576695f1375dbb789f062e97b9";
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
