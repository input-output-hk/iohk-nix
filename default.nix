{ config ? {}
, system ? builtins.currentSystem
}:

rec {
  commonLib = {
    fetchNixpkgs = import ./fetch-nixpkgs.nix;
    maybeEnv = import ./maybe-env.nix;
  };

  # Overlay which adds more functions to pkgs.lib
  extraLib = self: super: {
    lib = super.lib.extend (lib: _: {
      cleanSourceHaskell = import ./clean-source-haskell.nix { inherit lib; };
      getPackages = import ./get-packages.nix { inherit lib; };
      commitIdFromGitRepo = import ./commit-id.nix { inherit lib; };
    });
  };

  cardano-sl = rec {
    pkgs = import (commonLib.fetchNixpkgs ./cardano-sl/nixpkgs-src.json) {
      inherit config system;
      overlays = [
        (import ./overlays/jemalloc.nix)
        extraLib
      ];
    };
    haskellPackages = args: import ./haskell-packages.nix (args // { inherit pkgs; });
  };

  daedalus = {
    pkgs = import (commonLib.fetchNixpkgs ./daedalus/nixpkgs-src.json) {
      inherit config system;
      # overlays = [ extraLib ]; # needs nixpkgs-18.09
    };
  };

  tests = {
    hlint = ./tests/hlint.nix;
    shellcheck = ./tests/shellcheck.nix;
    stylishHaskell = ./tests/stylish-haskell.nix;
  };
}
