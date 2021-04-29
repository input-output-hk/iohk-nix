{ lib, haskell-nix, symlinkJoin }:

let
  project = mkProject {};
  mkProject = args: haskell-nix.cabalProject ({
    src = haskell-nix.haskellLib.cleanSourceWith {
      name = "iohk-nix-utils";
      src = ./.;
    };
    compiler-nix-name = "ghc8104";
    index-state = "2021-03-15T00:00:00Z";
  } // args);
in
  symlinkJoin {
    name = "iohk-nix-utils";
    paths = lib.attrValues project.iohk-nix-utils.components.exes;
    passthru = {
      inherit project mkProject;
      shell = project.shellFor {};
      roots = project.roots;
      package = builtins.trace "WARNING: iohk-nix `haskellBuildUtils.package` has been renamed to `haskellBuildUtils`." null;
      stackRebuild = builtins.trace "WARNING: iohk-nix stackRebuild script has been removed." null;
    };
  }
