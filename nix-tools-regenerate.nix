# A script for generating the nix haskell package set based on stackage,
# using the common convention for repo layout.

{ nix-tools, writeScript }:

writeScript "nix-tools-regenerate" ''
  # stack-to-nix will transform the stack.yaml file into something
  # nix can understand.
  ${nix-tools}/bin/stack-to-nix -o nix stack.yaml > nix/.stack-pkgs.nix
''
