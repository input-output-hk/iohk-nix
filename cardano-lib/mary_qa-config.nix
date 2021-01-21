##########################################################
###############         Mary QA            ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  ByronGenesisFile = ./mary_qa-byron-genesis.json;
  ByronGenesisHash = "98a97e7a45c116dc796a168cf1ab6004b487a0b39e5b13e77d9b1ef869206c4a";
  ShelleyGenesisFile = ./mary_qa-shelley-genesis.json;
  ShelleyGenesisHash = "acadf76c1d83ab3d65da935c7bf4853513e3a0b284c8b6d2e39f78a90ee04f14";

  ##### Core protocol parameters #####

  # This is the instance of the Ouroboros family that we are running.
  # The node also supports various test and mock instances.
  # "RealPBFT" is the real (ie not mock) (permissive) OBFT protocol, which
  # is what we use on mainnet in Byron era.
  Protocol = "Cardano";

  PBftSignatureThreshold = 0.9;
  # The mainnet does not include the network magic into addresses. Testnets do.
  RequiresNetworkMagic = "RequiresMagic";

  #TestShelleyHardForkAtEpoch = 1;
  #TestAllegraHardForkAtEpoch = 2;
  TestMaryHardForkAtEpoch = 0;

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
