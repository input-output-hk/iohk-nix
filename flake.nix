{
  description = "IOHK nix overlays";

  outputs = { self }: {

    overlays = {
      crypto = import ./overlays/crypto;
      haskell-nix-extra = import ./overlays/haskell-nix-extra;
      cardano-lib = (final: prev: {
        cardanoLib = final.callPackage ./cardano-lib {};
      });
    };

  };
}
