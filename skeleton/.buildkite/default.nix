{ iohkSkeletonPackages ? import ../default.nix {}
, pkgs ? iohkSkeletonPackages.pkgs
, commonLib ? iohkSkeletonPackages.commonLib
}:

commonLib.haskellBuildUtils.stackRebuild {
  script = ./rebuild.hs;
  buildTools = [];
  libs = ps: [];
  shell = import ../nix/stack-shell.nix {};
}
