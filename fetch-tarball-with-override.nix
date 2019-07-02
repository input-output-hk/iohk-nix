# overriding logic so we can pass -I to nix, and overide the
# relevant import. That is if we want to override for example
# `custom_nixpkgs` or `hackage`, we can simply provide -I hackage=/path/to/hackage.nix
# and have nix use that instead of the one we reference here.
# Same for haskell and stackage.
override: srcJson:
  let
    try = builtins.tryEval (builtins.findFile builtins.nixPath override);
  in if try.success then
    builtins.trace "using search host <${override}>" try.value
  else
    let
      spec = builtins.fromJSON (builtins.readFile srcJson);
    in builtins.fetchTarball {
      url = "${spec.url}/archive/${spec.rev}.tar.gz";
      inherit (spec) sha256;
    }
