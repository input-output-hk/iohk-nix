with import ./nix {};
stdenv.mkDerivation {
  name = "shell";
  buildInputs = [ commonLib.niv ];
}
