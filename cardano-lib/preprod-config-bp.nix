##########################################################
#######                  Preprod                  ########
####### Cardano Node Block Producer Configuration ########
##########################################################

import ./preprod-config.nix // {
  ##### Core protocol parameters #####
  PeerSharing = false;
  TargetNumberOfKnownPeers = 100;
  TargetNumberOfRootPeers = 100;
}
