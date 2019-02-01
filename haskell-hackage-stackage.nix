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
                    (pkgs.fetchFromGitHub { owner  = "input-output-hk";
                                            repo   = "hackage.nix";
                                            rev    = "1bee48237cb87d9ad6432a9ddb7777f858674400";
                                            sha256 = "0sllxmffrcvcq63wlvz64s0i0ls2249vwix645899k6xh159z5pj";
                                            name   = "hackage-exprs-source"; }))
                   ;
  # a different haskell infrastructure
  haskell = import (overrideWith "haskell"
                    (pkgs.fetchFromGitHub { owner  = "input-output-hk";
                                            repo   = "haskell.nix";
                                            rev    = "dae8025469d94e739aa52f23685bcd87840c72e3";
                                            sha256 = "0bj577aj2v7yl6d1l6rinp0bbn4k9m8k4jd46m4hspqaisfd58hg";
                                            name   = "haskell-lib-source"; }))
                   hackage;

  # the set of all stackage snapshots
  stackage = import (overrideWith "stackage"
                     (pkgs.fetchFromGitHub { owner  = "input-output-hk";
                                             repo   = "stackage.nix";
                                             rev    = "5ccfc7662469843768a5c4924d91faafbe5824e1";
                                             sha256 = "1zwasyscqn4751i10165imwj4715hh5arwmccqkpvpn9bnb6c5ck";
                                             name   = "stackage-snapshot-source"; }))
                   ;
}
