# The ledger snapshot policy, derived from a network's shelley genesis so it
# scales with the security parameter instead of being restated per network.
#
# Takes a path to shelley-genesis.json and returns a `LedgerDB.Snapshots`
# attrset in the flat spelling POM reads.  `envelope.nix` renames
# `SnapshotInterval` and `SlotOffset` for the enveloped form.  Override at the
# call site where a network needs something different.
genesisFile: let
  inherit (builtins) floor fromJSON readFile;

  genesis = fromJSON (readFile genesisFile);
  inherit (genesis) securityParam slotLength;

  # The interval is in slots, the delays in seconds.
  seconds = slots: floor (slots * slotLength);

  # Snapshot every 40k slots.
  interval = securityParam * 40;

  # Spread writes over the first quarter of each interval so a fleet does not
  # snapshot in lockstep.  Upstream defaults this to a flat 21600s, which is
  # 10k for mainnet only and exceeds the whole interval on a smaller network,
  # letting a delayed write land after the next snapshot is due.
  maxDelay = seconds (securityParam * 10);

  # POM fails the parse on MinDelay > MaxDelay, so clamp rather than let a
  # small enough network produce an inverted range.
  minDelay = if maxDelay < 300 then maxDelay else 300;
in {
  SnapshotInterval = interval;
  SlotOffset = 0;
  MinDelay = minDelay;
  MaxDelay = maxDelay;
  NumOfDiskSnapshots = 2;
}
