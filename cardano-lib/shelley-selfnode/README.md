# Shelley Genesis File Creation

```
cardano-cli shelley genesis key-gen-genesis --verification-key-file genesis.vkey --signing-key-file genesis.skey
cardano-cli shelley genesis key-gen-delegate --verification-key-file deleg.vkey --signing-key-file deleg.skey --operational-certificate-issue-counter deleg.counter
cardano-cli shelley genesis key-gen-utxo --verification-key-file utxo.vkey --signing-key-file utxo.skey
GENESIS_HASH=$(cardano-cli shelley genesis key-hash --verification-key-file genesis.vkey)
DELEG_HASH=$(cardano-cli shelley genesis key-hash --verification-key-file deleg.vkey)
UTXO_ADDR=$(cardano-cli shelley genesis initial-addr --verification-key-file utxo.vkey)

```

See genesis.json for example genesis file.

Replace `GenDelegs` with `"${GENESIS_HASH}": "${DELEG_HASH}"`
Replace `InitialFunds` with `"${UTXO_ADDR}": 1000000000000



# Stake Pool Secret Creation

If embedding in genesis file, signing key file is deleg.skey and deleg.counter from above step.

Otherwise, it's the delegation key used for generation stake pool registration (not implemented yet).

```
cardano-cli shelley node key-gen-VRF --verification-key-file node-vrf.vkey --signing-key-file node-vrf.skey
cardano-cli shelley node key-gen-KES --verification-key-file node-kes.vkey --signing-key-file node-kes.skey --kes-duration 90
cardano-cli shelley node issue-op-cert --verification-key-file node-kes.vkey --signing-key-file deleg.skey --operational-certificate-issue-counter deleg.counter --kes-period 0 --out-file node.opcert
```

# Running the node

The simplest way to run the node is run `scripts.shelley_selfnode.node` in the cardano-node repository.

To run manually:

```
cardano-node run --config config.yaml --database-path db-shelley_selfnode --socket-path node.socket --topology topology.yaml --host-addr 127.0.0.1 --port 3001 --shelley-vrf-key node-vrf.skey --shelley-kes-key node-kes.skey --shelley-operational-certificate node.opcert
```
