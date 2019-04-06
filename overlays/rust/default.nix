self: super: let
  stableChannelToml = ./channel-rust-stable.toml;
  stableChannel = super.lib.rustLib.fromManifestFile stableChannelToml {
    inherit (super) stdenv fetchurl patchelf;
  };

in {
  rust = {
    rustc = stableChannel.rust;
    cargo = stableChannel.cargo;
  };
  rustPlatform = super.recurseIntoAttrs (super.makeRustPlatform {
    rustc = stableChannel.rust;
    cargo = stableChannel.cargo;
  });
  jormungandr = super.pkgs.callPackage ./jormungandr.nix {};
  cardano-http-bridge = super.pkgs.callPackage ./cardano-http-bridge.nix {};
  cardano-cli = super.pkgs.callPackage ./cardano-cli.nix {};
}

