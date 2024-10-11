##########################################################
#######                  Private                  ########
####### Cardano Node Block Producer Configuration ########
##########################################################

import ./private-config.nix // {
  ##### Core protocol parameters #####
  PeerSharing = false;
  TargetNumberOfKnownPeers = 100;
  TargetNumberOfRootPeers = 100;
}
