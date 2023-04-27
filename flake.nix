{
  description = "IOHK nix lib, packages and overlays";

  inputs.nixpkgs.url = "github:nixos/nixpkgs?ref=release-22.11";

  outputs = { self, nixpkgs }: rec {

    lib = import ./lib nixpkgs.lib;

    overlays = {
      crypto = import ./overlays/crypto;
      haskell-nix-extra = import ./overlays/haskell-nix-extra;
      cardano-lib = (final: prev: {
        cardanoLib = final.callPackage ./cardano-lib {};
      });
      utils = import ./overlays/utils;
    };

    cabal-wrapper = ./pkgs/cabal-wrapper.nix;

    checks = {
      hlint = ./tests/hlint.nix;
      shell = ./tests/shellcheck.nix;
      stylish-haskell = ./tests/stylish-haskell.nix;
    };

    utils = {
      cabal-project = ./ci/cabal-project-regenerate;
      ciJobsAggregates = ./ci/aggregates.nix;
    };

    pkgs = import nixpkgs { system = "x86_64-linux"; overlays = builtins.attrValues overlays; };
    darwin-pkgs = import nixpkgs { system = "x86_64-darwin"; overlays = builtins.attrValues overlays; };


    # we can use this, to get a coherent picture of the sources for
    # the various libraries.  The following command will produce a
    # JSON output, that contains each of our libs, with their respective
    # versions.
    #
    #    nix eval --json .#lib-srcs
    #
    lib-srcs = {
      secp256k1 = pkgs.secp256k1.src.url;
      sodium    = pkgs.libsodium-vrf.src.url;
      blst      = pkgs.libblst.src.url;
    };

    dist = let
      # For packaging, we can'd deal with split outputs.
      mkSingleOutput = drv: drv.overrideDerivation (drv': { outputs = [ "out" ]; });
      mkDebianPkg = prefix: drv: let
        control = pkgs.writeText "control" ''
        Package: ${drv.pname}
        Version: ${if drv.version == "unstable-2022-02-06" then "2022-02-06-unstable" else drv.version}
        Architecture: amd64
        Maintainer: IOG <engineering@iog.io>
        Description: ${drv.meta.description}
        '';
      in pkgs.stdenv.mkDerivation {
        inherit (drv) version meta;
        name = "${drv.name}-debian-pkg";
        phases = [ "buildPhase" "installPhase" ];
        buildInputs = with pkgs; [ rsync ];
        buildPhase = ''
          mkdir -p .${prefix}
          rsync -a ${drv}/ .${prefix}

          ls -lah

          # replace any reference to the nix-path in the pkg-config files with
          # references to the target prefix

          for pc in $(find .${prefix} -name "*.pc"); do
            substituteInPlace $pc --replace "${drv}" "${prefix}"
            cat $pc
          done

          # create the data.tar.gz containing the install tree; we need to
          # have data.tar.gz exist in . or tar will complain that . changed
          # while creating the archive.
          touch data.tar.gz
          tar --exclude=env-vars --exclude=data.tar.gz -czf data.tar.gz .

          # create the minimal control file, and control.tar.gz
          substituteAll ${control} control
          tar czf control.tar.gz control

          # create the debian-binary file
          echo 2.0 > debian-binary

          # package it up. It's just ar.
          ar r ${drv.name}.deb debian-binary control.tar.gz data.tar.gz
        '';
        installPhase = ''
          mkdir -p $out
          mv ${drv.name}.deb $out/

          # make it downloadable from hydra.
          mkdir -p $out/nix-support
          for f in $out/*.deb; do
            echo "file binary-dist \"''${f}\"" \
                >> $out/nix-support/hydra-build-products
          done
        '';
      };
      mkDarwinPkg = prefix: drv: let
        PackageInfo = pkgs.writeText "PackageInfo" ''
        <?xml version="1.0" encoding="utf-8" standalone="no"?>
        <pkg-info identifier="io.iog.${drv.pname}" version="${drv.version}" format-version="2" auth="root" install-location="${prefix}">
          <payload numberOfFiles="@numfiles@" installKBytes="@kbsize@"/>
          <scripts>
            <postinstall file="./postinstall"/>
          </scripts>
        </pkg-info>
        '';
        PostInstall = pkgs.writeText "PostInstall" ''
        #!/bin/bash
        for lib in @libs@; do
          chmod +w "${prefix}/$lib"
          install_name_tool -id "${prefix}/$lib" "${prefix}/$lib"
          chmod -w "${prefix}/$lib"
        done
        '';
      in pkgs.stdenv.mkDerivation {
        inherit (drv) version meta;
        name = "${drv.name}-macos-pkg";
        phases = [ "buildPhase" "installPhase" ];
        buildInputs = with pkgs; [ rsync xar cpio bomutils ];
        buildPhase = ''
          mkdir -p pkg
          rsync -a ${drv}/ pkg

          # replace any reference to the nix-path in the pkg-config files with
          # references to the target prefix

          for pc in $(find pkg -name "*.pc"); do
            substituteInPlace $pc --replace "${drv}" "${prefix}"
            cat $pc
          done

          export numfiles=$(find pkg/ |wc -l)
          export kbsize=$(du -ks pkg/ |cut -f1)

          substituteAll ${PackageInfo} PackageInfo

          (cd pkg; find . | cpio -o --format odc --owner 0:80 | gzip -c ) > Payload
          mkbom -u 0 -g 80 pkg/ Bom

          mkdir -p scripts

          # ensure we drop the ./ from the found results, by using cut.
          export libs=$(cd pkg; find . -name "*.dylib" -type f | cut -c 3-)

          substituteAll ${PostInstall} scripts/postinstall
          chmod +x scripts/postinstall

          (cd scripts; find . | cpio -o --format odc --owner 0:80 | gzip -c ) > Scripts

          xar --compression none -cf "${drv.name}.pkg" * --exclude="${drv.name}.pkg" --exclude=pkg/ --exclude=scripts/

        '';
        installPhase = ''
          mkdir -p $out
          mv ${drv.name}.pkg $out/

          # make it downloadable from hydra.
          mkdir -p $out/nix-support
          for f in $out/*.pkg; do
            echo "file binary-dist \"''${f}\"" \
                >> $out/nix-support/hydra-build-products
          done
        '';
      };
      mkPacmanPkg = prefix: drv: let
        pkgrel = 1; # pkg release version
        PKGINFO = pkgs.writeText ".PKGINFO" ''
        # Generated by packaging.nix
        pkgname = mingw-w64-x86_64-${drv.pname}
        pkgbase = mingw-w64-${drv.pname}
        pkgver = ${drv.version}-${toString pkgrel}
        pkgdesc = ${drv.meta.description} (mingw-w64)
        url = ${drv.meta.homepage}
        builddate = @date@
        packager = packaging.nix
        size = @size@
        arch = any
        license = custom:ISC
        '';
      in pkgs.stdenv.mkDerivation {
        inherit (drv) version meta;
        name = "${drv.name}-msys2-pkg";
        phases = [ "buildPhase" "installPhase" ];
        buildInputs = with pkgs; [ rsync tree libarchive zstd ];
        buildPhase = ''
          mkdir -p .${prefix}
          rsync -a ${drv}/ .${prefix}

          # replace any reference to the nix-path in the pkg-config files with
          # references to the target prefix

          for pc in $(find .${prefix} -name "*.pc"); do
            substituteInPlace $pc --replace "${drv}" "${prefix}"
            cat $pc
          done

          # this date is of course not correct, but we don't want the derivation
          # to mutate all the time, just because it was re-built.
          export date=0
          export size=$(du -bs $out|cut -f1)

          substituteAll ${PKGINFO} .PKGINFO

          list_package_files() {
            (
              export LC_COLLATE=C
              shopt -s dotglob globstar
              # bash 5.0 only works with combo directory + file globs
              printf '%s\0' **/*
            )
          }

          list_package_files | LANG=C bsdtar -cnf - --format=mtree \
        		--options='!all,use-set,type,uid,gid,mode,time,size,sha256,link' \
        		--null --files-from - \
            --exclude env-vars --exclude .MTREE \
            | gzip -c -f -n > .MTREE

          list_package_files | LANG=C bsdtar --no-fflags --no-read-sparse -cnf - \
            --null --files-from - \
            --exclude env-vars --exclude ${drv.name}.pkg.tar.zstd \
            | zstd > ${drv.name}.pkg.tar.zstd
        '';
        installPhase = ''
          mkdir -p $out
          mv ${drv.name}.pkg.tar.zstd $out/

          # make it downloadable from hydra.
          mkdir -p $out/nix-support
          for f in $out/*.zstd; do
            echo "file binary-dist \"''${f}\"" \
                >> $out/nix-support/hydra-build-products
          done
        '';
      };
    in {
      msys2 = {
        libsodium-vrf= mkPacmanPkg "/mingw64/opt/cardano" (mkSingleOutput pkgs.pkgsCross.mingwW64.libsodium-vrf);
        libsodium    = mkPacmanPkg "/mingw64/opt/cardano" (mkSingleOutput pkgs.pkgsCross.mingwW64.libsodium);
        libblst      = mkPacmanPkg "/mingw64/opt/cardano" (mkSingleOutput pkgs.pkgsCross.mingwW64.libblst);
        libsecp256k1 = mkPacmanPkg "/mingw64/opt/cardano" (mkSingleOutput pkgs.pkgsCross.mingwW64.secp256k1);
      };
      macos = {
        libsodium-vrf= mkDarwinPkg "/usr/local/opt/cardano" (mkSingleOutput darwin-pkgs.libsodium-vrf);
        libsodium    = mkDarwinPkg "/usr/local/opt/cardano" (mkSingleOutput darwin-pkgs.libsodium);
        libblst      = mkDarwinPkg "/usr/local/opt/cardano" (mkSingleOutput darwin-pkgs.libblst);
        libsecp256k1 = mkDarwinPkg "/usr/local/opt/cardano" (mkSingleOutput darwin-pkgs.secp256k1);
      };
      debian = {
        libsodium-vrf= mkDebianPkg "/usr/local/opt/cardano" (mkSingleOutput pkgs.libsodium-vrf);
        libsodium    = mkDebianPkg "/usr/local/opt/cardano" (mkSingleOutput pkgs.libsodium);
        libblst      = mkDebianPkg "/usr/local/opt/cardano" (mkSingleOutput pkgs.libblst);
        libsecp256k1 = mkDebianPkg "/usr/local/opt/cardano" (mkSingleOutput pkgs.secp256k1);
      };
    };
    hydraJobs = dist;
  };
}
