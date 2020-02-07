{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, sourcesOverride ? {}
}:
let
  # use default stable nixpkgs from iohk-nix instead of our own:
  sources = removeAttrs (import ./sources.nix) [ "nixpkgs" ]
    // sourcesOverride;

  # for inclusion in pkgs:
  nixpkgsOverlays = [
    (pkgs: _: with pkgs; {

      # commonLib: mix pkgs.lib with iohk-nix utils and our own:
      commonLib = lib // iohkNix //
        import ./util.nix { inherit haskell-nix; };

      # Example of using a package from iohk-nix
      # TODO: Declare packages required by the build.
      inherit (iohkNix.jormungandrLib.packages.release) jormungandr;

      # Our haskell-nix-ified cabal project:
      haskellNixPackages = import ./haskell.nix {
        inherit
          config
          lib
          stdenv
          haskell-nix
          buildPackages
          makeWrapper
          jormungandr
          cowsay;
      };
    })
  ];

  # IOHK pkgs that include haskell-nix overlays, using our sources as override:
in (import sources.iohk-nix {
    inherit system crossSystem config nixpkgsOverlays;
    sourcesOverride = sources;
  }).pkgs
