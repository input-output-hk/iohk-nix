{
  description = "IOHK nix overlays";

  outputs = { self }: {

    overlays = {
      crypto = import ./overlays/crypto;
      haskell-nix-extra = import ./overlays/haskell-nix-extra;
    };

  };
}
