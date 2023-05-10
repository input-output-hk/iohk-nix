# Generate new testnet

```
cardano-cli genesis create-cardano --genesis-dir . --gen-genesis-keys 3 --supply 30000000000000000  --testnet-magic 42  --slot-coefficient 0.05 --byron-template byron.json --shelley-template shelley.json --alonzo-template alonzo.json --conway-template conway.json --security-param 108 --slot-length 1000 --node-config-template config.json
```
