{ lib, haskell-nix, symlinkJoin
, compiler-nix-name, index-state
, evalSystem ? null }:

let
  project = mkProject {};

  evalSystemArg = if evalSystem == null
    then {}
    else { inherit evalSystem; };
  mkProject = args: haskell-nix.cabalProject ({
    src = haskell-nix.haskellLib.cleanSourceWith {
      name = "iohk-nix-utils";
      src = ./.;
    };
    inherit compiler-nix-name index-state;
  } // evalSystemArg // args);
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
