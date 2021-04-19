{ lib, haskell-nix, symlinkJoin }:

let
  project = haskell-nix.stackProject {
    src = haskell-nix.cleanSourceHaskell {
      name = "iohk-nix-utils-src";
      src = ./.;
    };
  };
in
  symlinkJoin {
    name = "iohk-nix-utils";
    paths = lib.attrValues project.iohk-nix-utils.components.exes;
    passthru = {
      shell = project.shellFor {};
      roots = project.roots;
      package = builtins.trace "WARNING: iohk-nix `haskellBuildUtils.package` has been renamed to `haskellBuildUtils`." null;
      stackRebuild = builtins.trace "WARNING: iohk-nix stackRebuild script has been removed." null;
    };
  }
