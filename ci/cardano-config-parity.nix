# Check that `mkEnvelope` still reproduces `cardano-config migrate`.
#
# Deliberately not a flake output.  The binary is a content-addressed closure,
# so reaching it calls `builtins.fetchClosure`, which is an experimental
# feature.  As a flake output that cost lands on everyone: a `hydraJobs` entry
# makes `nix flake check` fail without the feature, and any output at all adds a
# lock entry for every downstream consumer.  Neither is a fair price for a check
# that only this repository's CI runs, so the job lives here and the GitHub
# workflow builds it by path.  Run it the same way locally:
#
#     nix build --no-link -f ci/cardano-config-parity.nix
#
# The closure is pinned rather than taken from the capkgs flake, for the same
# reason: a flake input would be locked by every consumer whether they evaluate
# this or not.  `fromPath` comes from capkgs' `packages.json`, under the key
# `github:IntersectMBO/cardano-config/<rev>#packages.x86_64-linux.cardano-config-exe`.
# Input-addressed, as capkgs' own flake fetches it.
#
# Bump this with the `cardano-config` flake input.  The assertion below is what
# keeps the two together: a store path is opaque, so without it a bumped source
# pin and a forgotten binary would compare against different releases and the
# failure would be read as a parity regression rather than a stale pin.
{iohk-nix ? import ../. {}}: let
  cardanoConfigExe = builtins.fetchClosure {
    fromStore = "https://cache.iog.io";
    fromPath = /nix/store/ipg0p99a09rs2z5dbgg7qvyydm2vbhm2-cardano-config-exe-cardano-config-2.2.1.0;
    inputAddressed = true;
  };

  # The release the schemas come from, as the flake input records it.  Its tags
  # are named `cardano-config-<version>`, which is also how the binary's store
  # path ends, so one is checkable against the other.
  sourceTag = (builtins.fromJSON (builtins.readFile ../flake.lock)).nodes."cardano-config".original.ref;

  # A store path's basename is `<32 character hash>-<name>`, and it is the name
  # that carries the release.
  exeName = builtins.substring 33 (-1) (baseNameOf cardanoConfigExe);
  expected = "cardano-config-exe-${sourceTag}";
in
  assert
  exeName == expected
  || throw ("ci/cardano-config-parity.nix: the pinned cardano-config binary is ${exeName},"
    + " and the cardano-config flake input is ${sourceTag}.  Bump `fromPath` to the"
    + " closure capkgs built for that release, from its packages.json.");
    iohk-nix.cardanoLib.mkConfigParity cardanoConfigExe iohk-nix.cardanoLib.parityTargets
