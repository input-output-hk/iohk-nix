##########################################################
#######                 Sanchonet                 ########
####### Cardano Node Block Producer Configuration ########
##########################################################

import ./sanchonet-config.nix // {
  ##### Core protocol parameters #####
  PeerSharing = false;
  TargetNumberOfKnownPeers = 100;
  TargetNumberOfRootPeers = 100;
}
