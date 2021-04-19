{ lib, haskell-nix, symlinkJoin }:

let
  project = haskell-nix.cabalProject {
    src = haskell-nix.haskellLib.cleanGit {
      name = "iohk-nix-utils-src";
      src = ./.;
    };
    compiler-nix-name = "ghc8104";
    index-state = "2021-03-15T00:00:00Z";
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
