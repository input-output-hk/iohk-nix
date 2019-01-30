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
                                            rev    = "dae8025469d94e739aa52f23685bcd87840c72e3";
                                            sha256 = "0bj577aj2v7yl6d1l6rinp0bbn4k9m8k4jd46m4hspqaisfd58hg";
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
