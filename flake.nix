{
  description = "IOHK nix overlays";

  outputs = { self }: {

    overlays = {
      crypto = import ./overlays/crypto;
    };

  };
}
