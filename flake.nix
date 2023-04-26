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
      mkPacmanPkg = prefix: drv: let
        PKGINFO = pkgs.writeText ".PKGINFO" ''
        # Generated by packaging.nix
        pkgname = mingw-w64-x86_64-${drv.pname}
        pkgbase = mingw-w64-${drv.pname}
        pkgver = ${drv.version}
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

          for pc in $(find $out -name "*.pc"); do
            substituteInPlace $pc --replace "${drv}" "${prefix}"
            cat $pc
          done

          export date=$(date +%s)
          export size=$(du -bs $out|cut -f1)

          substituteAll ${PKGINFO} .PKGINFO
          cat .PKGINFO

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
        '';
      };
    in {
      msys2 = {
        libsodium    = mkPacmanPkg "/mingw64/opt/cardano" (mkSingleOutput pkgs.pkgsCross.mingwW64.libsodium-vrf);
        libblst      = mkPacmanPkg "/mingw64/opt/cardano" (mkSingleOutput pkgs.pkgsCross.mingwW64.libblst);
        libsecp256k1 = mkPacmanPkg "/mingw64/opt/cardano" (mkSingleOutput pkgs.pkgsCross.mingwW64.secp256k1);
      };
    };
  };
}
