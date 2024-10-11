##########################################################
#######                 Shelley QA                ########
####### Cardano Node Block Producer Configuration ########
##########################################################

import ./shelley_qa-config.nix // {
  ##### Core protocol parameters #####
  PeerSharing = false;
  TargetNumberOfKnownPeers = 100;
  TargetNumberOfRootPeers = 100;
}
