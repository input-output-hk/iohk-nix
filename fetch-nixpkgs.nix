srcJson: let
  spec = builtins.fromJSON (builtins.readFile srcJson);
in builtins.fetchTarball {
  url = "${spec.url}/archive/${spec.rev}.tar.gz";
  inherit (spec) sha256;
}
