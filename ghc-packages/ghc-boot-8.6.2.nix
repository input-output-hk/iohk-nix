{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.22";
      identifier = { name = "ghc-boot"; version = "8.6.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "ghc-devs@haskell.org";
      author = "";
      homepage = "";
      url = "";
      synopsis = "Shared functionality between GHC and its boot libraries";
      description = "This library is shared between GHC, ghc-pkg, and other boot\nlibraries.\n\nA note about \"GHC.PackageDb\": it only deals with the subset of\nthe package database that the compiler cares about: modules\npaths etc and not package metadata like description, authors\netc. It is thus not a library interface to ghc-pkg and is *not*\nsuitable for modifying GHC package databases.\n\nThe package database format and this library are constructed in\nsuch a way that while ghc-pkg depends on Cabal, the GHC library\nand program do not have to depend on Cabal.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.binary)
          (hsPkgs.bytestring)
          (hsPkgs.directory)
          (hsPkgs.filepath)
          (hsPkgs.ghc-boot-th)
          ];
        };
      };
    } // rec { src = pkgs.fetchurl { url = http://releases.mobilehaskell.org/ghc-packages/ghc-boot-8.6.2.tar.gz; sha256 = "0y96yqrbcjg6l5l2y9sm2llyk37zrzg2bb0lj3k16ld29j05g1gw"; }; }
