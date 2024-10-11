##########################################################
#######                  Preview                  ########
####### Cardano Node Block Producer Configuration ########
##########################################################

import ./preview-config.nix // {
  ##### Core protocol parameters #####
  PeerSharing = false;
  TargetNumberOfKnownPeers = 100;
  TargetNumberOfRootPeers = 100;
}
