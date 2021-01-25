##########################################################
###############         Mary QA            ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./mary_qa-byron-genesis.json;
  ByronGenesisHash = "5fabaf2188b1b2a0dde0046ad564ca37c5d92e67b6d7e92da72b2bbc35dfed7e";
  ShelleyGenesisFile = ./mary_qa-shelley-genesis.json;
  ShelleyGenesisHash = "93cfc4fcb0061ae69960efb328d88cf0ab3d709049c0675714c1dd41312d7be7";

  ##### Core protocol parameters #####

  # This is the instance of the Ouroboros family that we are running.
  # The node also supports various test and mock instances.
  # "RealPBFT" is the real (ie not mock) (permissive) OBFT protocol, which
  # is what we use on mainnet in Byron era.
  Protocol = "Cardano";

  PBftSignatureThreshold = 0.9;
  # The mainnet does not include the network magic into addresses. Testnets do.
  RequiresNetworkMagic = "RequiresMagic";

  TestShelleyHardForkAtEpoch = 1;
  TestAllegraHardForkAtEpoch = 2;
  TestMaryHardForkAtEpoch = 3;

  ##### Update system parameters #####

  # This protocol version number gets used by block producing nodes as part
  # part of the system for agreeing on and synchronising protocol updates.
  LastKnownBlockVersion-Major = 4;
  LastKnownBlockVersion-Minor = 0;
  LastKnownBlockVersion-Alt = 0;

  # In the Byron era some software versions are also published on the chain.
  # We do this only for Byron compatibility now.
  ApplicationName = "cardano-sl";
  ApplicationVersion = 0;
}
