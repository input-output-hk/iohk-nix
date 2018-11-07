haskellPackages: filter:
  let
    cusutmPackages = lib.filterAttrs (name: drv: filter name) haskellPackages;
  in
    customPackages
