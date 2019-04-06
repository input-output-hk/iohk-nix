self: super: {
  rust = {
    rustc = super.rustChannels.stable.rust;
    cargo = super.rustChannels.stable.cargo;
  };
  rustPlatform = super.recurseIntoAttrs (super.makeRustPlatform {
    rustc = super.rustChannels.stable.rust;
    cargo = super.rustChannels.stable.cargo;
  });
  jormungandr = super.pkgs.callPackage ./jormungandr.nix {};
  cardano-http-bridge = super.pkgs.callPackage ./cardano-http-bridge.nix {};
  cardano-cli = super.pkgs.callPackage ./cardano-cli.nix {};
}

