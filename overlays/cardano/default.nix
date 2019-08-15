self: super: let
  wallet = import (self.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-wallet";
    rev = "1833df1a640d597382a7c018ca9bffeca4cf698c";
    sha256 = "0b4i8wy3w192vbr6wb4j9d23smxycxkxa7qrwxr3djk01x5is9d0";
  }) {};
  cardano-wallet-jormungandr = wallet.cardano-wallet-jormungandr;
  cardano-wallet-http-bridge = wallet.cardano-wallet-http-bridge;

in {
  inherit cardano-wallet-jormungandr cardano-wallet-http-bridge;
}
