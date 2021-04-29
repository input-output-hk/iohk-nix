{ lib, haskell-nix, symlinkJoin
, compiler-nix-name, index-state }:

let
  project = mkProject {};
  mkProject = args: haskell-nix.cabalProject ({
    src = haskell-nix.haskellLib.cleanSourceWith {
      name = "iohk-nix-utils";
      src = ./.;
    };
    inherit compiler-nix-name index-state;
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
