{ lib, runCommand, fetchFromGitHub, git, makeWrapper, haskell }:

let
  src = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-repo-tool";
    rev = "71f21aa44dd491b34a10868a0de02ef43f788437";
    sha256 = "1grmqk22q6gyk5qivp8zxklvcd43p10wbvsayy34z4mcg2745qiy";
  };

  # pkgSetCabal = haskell.mkCabalProjectPkgSet {
  #   plan-pkgs = import (haskell.callCabalProjectToNix {
  #     inherit src;
  #     index-state = "2019-08-01T00:00:00Z";
  #   });
  # };

  pkgSet = haskell.mkStackPkgSet {
    stack-pkgs = (haskell.importAndFilterProject (haskell.callStackToNix {
      inherit src;
    })).pkgs;
    pkg-def-extras = [];
    modules = [{
      packages.cardano-repo-tool.components.exes.cardano-repo-tool = {
        build-tools = [ makeWrapper ];
        postInstall = ''
          wrapProgram $out/bin/cardano-repo-tool \
            --prefix PATH : ${git}/bin
        '';
      };
      packages.cardano-repo-tool.components.all.postInstall = lib.mkForce "";
    }];
  };

  packages = pkgSet.config.hsPkgs;

  inherit (packages.cardano-repo-tool.components.exes) cardano-repo-tool;

in
  cardano-repo-tool
