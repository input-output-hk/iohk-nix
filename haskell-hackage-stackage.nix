{ pkgs }:
let
  # overriding logic so we can pass -I to nix, and overide the
  # relevant import.
  overrideWith = override: default:
   let
     try = builtins.tryEval (builtins.findFile builtins.nixPath override);
   in if try.success then
     builtins.trace "using search host <${override}>" try.value
   else
     default;
in rec {
  # all packages from hackage as nix expressions
  hackage = import (overrideWith "hackage"
                    (pkgs.fetchFromGitHub { owner  = "angerman";
                                            repo   = "hackage.nix";
                                            rev    = "d4ef2536554d78c6adf368816a4476bc909fcd96";
                                            sha256 = "15siby9iwgi8bhkj83q1djvg6b6n4gj08n7yy66ccjck7fw4hbr8";
                                            name   = "hackage-exprs-source"; }))
                   ;
  # a different haskell infrastructure
  haskell = import (overrideWith "haskell"
                    (pkgs.fetchFromGitHub { owner  = "angerman";
                                            repo   = "haskell.nix";
                                            rev    = "ed456599a3a52592ffede765e9d12a953b77d606";
                                            sha256 = "0w7dg50y30zm7k5ilw0mhkggzhmzpb7x8cl2b82iizvscx6xffzc";
                                            name   = "haskell-lib-source"; }))
                   hackage;

  # the set of all stackage snapshots
  stackage = import (overrideWith "stackage"
                     (pkgs.fetchFromGitHub { owner  = "angerman";
                                             repo   = "stackage.nix";
                                             rev    = "f58d5b78e7a40260c6142c79e52c2bf3ae9876b9";
                                             sha256 = "1nd9lfm016rlhw3133488f8v8x3lbxrld422gw8gcjhhfls3civn";
                                             name   = "stackage-snapshot-source"; }))
                   ;
}
