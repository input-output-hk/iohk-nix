##########################################################
#######                  Mainnet                  ########
####### Cardano Node Block Producer Configuration ########
##########################################################

import ./mainnet-config.nix // {
  ##### Core protocol parameters #####
  PeerSharing = false;
  TargetNumberOfKnownPeers = 100;
  TargetNumberOfRootPeers = 100;
}
