self: super: let
  wallet = import (self.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-wallet";
    rev = "v2019-07-24";
    sha256 = "11j3an4i6kvk9h71hz1cngns2a47cmv38bsr3qyagcr1829rlrxl";
  }) {};
  cardano-wallet-jormungandr = wallet.cardano-wallet-jormungandr;
  cardano-wallet-http-bridge = wallet.cardano-wallet-http-bridge;

in {
  inherit cardano-wallet-jormungandr cardano-wallet-http-bridge;
}
