let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs { };
  iohk-nix = import ./. { };
  inherit (nixpkgs.lib) flatten mapAttrs;

in {
  jormungandr = nixpkgs.recurseIntoAttrs (mapAttrs (name: env:
    nixpkgs.recurseIntoAttrs {
      inherit (env.packages) jcli jcli-debug jormungandr jormungandr-debug;
    }) iohk-nix.jormungandrLib.environments);
}
