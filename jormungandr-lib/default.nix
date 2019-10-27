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
    echo "${environment.genesisHash}" > $out/genesis-hash.txt
    echo "file binary-dist $out/config.yaml" > $out/nix-support/hydra-build-products
    echo "file binary-dist $out/genesis-hash.txt" >> $out/nix-support/hydra-build-products

  '';
  environments = {
    stable = {
      genesisHash = "adbdd5ede31637f6c9bad5c271eec0bc3d0cb9efb86a5b913bb55cba549d0770";
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
      genesisHash = "ae57995b8fe086ba590c36dc930f2aa9b52b2ffa92c0698fff2347adafe8dc65";
      trustedPeers = [
        {
          address = "/ip4/13.230.137.72/tcp/3000";
          id = "ed25519_pk1w6f2sclsauhfd6r9ydgvn0yvpvg4p3x3u2m2n7thknwghrfpdu5sgvrql9";
        }
        {
          address = "/ip4/13.230.48.191/tcp/3000";
          id = "ed25519_pk1lzrdh0pcmhwcnqdl5cgcu7n0c76pm7g7p6pdey7wup54vz32gy6qlz5vnq";
        }
        {
          address = "/ip4/18.196.168.220/tcp/3000";
          id = "ed25519_pk1uufkgu0t9xm8ry04wnddtnku5gjg8typf5z6ehh65uc6nz4j8n4spq0xrl";
        }
        {
          address = "/ip4/3.124.132.123/tcp/3000";
          id = "ed25519_pk14tqkqnz3eydn0c8c8gmmyzxgnf2dztpy5dnrx09mhfzv0dh93s3qszqgpc";
        }
        {
          address = "/ip4/18.184.181.30/tcp/3000";
          id = "ed25519_pk178ge2jn6c40vgmrewgmg26nmtda47nk2jncukzj327mp3a9g2qzss2d44f";
        }
        {
          address = "/ip4/184.169.162.15/tcp/3000";
          id = "ed25519_pk1nk0ne8ez66w5tp2g8ctcakthjpz89eveyg0egcpylenhet83n0sq2jqz8q";
        }
        {
          address = "/ip4/13.56.87.134/tcp/3000";
          id = "ed25519_pk1ce450zrtn04eaevcn9csz0thpjuhxrysdrq6qlr9pq7e0wd842nsxy6r5k";
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
