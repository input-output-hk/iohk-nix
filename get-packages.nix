{ lib }: { haskellPackages, filter }:
  lib.filterAttrs (name: drv: filter name) haskellPackages
