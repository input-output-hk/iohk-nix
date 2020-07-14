##########################################################
###############     Selfnode Shelley       ###############
############### Cardano Node Configuration ###############
##########################################################

{
  ##### Locations #####

  GenesisFile = ./shelley_selfnode-shelley-genesis.json;

  ##### Core protocol parameters #####

  # This is the instance of the Ouroboros family that we are running.
  # The node also supports various test and mock instances.
  # "TPraos" is the shelley protocol.
  Protocol = "TPraos";

}
