self: super: let
  # bump mozilla-nixpkgs rev and run:
  # curl https://static.rust-lang.org/dist/channel-rust-stable.toml -o overlays/rust/channel-rust-stable.toml
  # to bump the rust stable version to latest
  stableChannelToml = ./channel-rust-stable.toml;
  stableChannel = super.lib.rustLib.fromManifestFile stableChannelToml {
    inherit (super) stdenv fetchurl patchelf;
  };

in {
  rust.packages.stable.rustc = stableChannel.rust;
  rust.packages.stable.cargo = stableChannel.cargo;
  rustPlatform = super.recurseIntoAttrs (super.rust.makeRustPlatform {
    rustc = stableChannel.rust;
    cargo = stableChannel.cargo;
  });
  makeJormungandr = (super.pkgs.callPackage ./jormungandr.nix {}).makeJormungandr;
  makeJcli = (super.pkgs.callPackage ./jormungandr.nix {}).makeJcli;
  makeJormungandr-debug = (super.pkgs.callPackage ./jormungandr.nix { buildType = "debug"; }).makeJormungandr;
  makeJcli-debug = (super.pkgs.callPackage ./jormungandr.nix { buildType = "debug"; }).makeJcli;
  cardano-http-bridge = super.pkgs.callPackage ./cardano-http-bridge.nix {};
  cardano-http-bridge-emurgo = super.pkgs.callPackage ./cardano-http-bridge-emurgo.nix {};
  cardano-cli = super.pkgs.callPackage ./cardano-cli.nix {};
}
