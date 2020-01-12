{ rustPlatform
, lib
, stdenv
, fetchFromGitHub
, sqlite
, protobuf
, pkgconfig
, openssl
, systemd
, darwin
, rustup
, rustc
, cargo
, rustfmt
, callPackage
, buildPackages
, buildType ? "release"
, ... }@args:
let
  fetchSrc = { name, rev, sha256 }:
    fetchFromGitHub {
      owner = "input-output-hk";
      repo = "jormungandr";
      inherit name rev sha256;
      fetchSubmodules = true;
    };

  Security = darwin.apple_sdk.frameworks.Security;

  ccForBuild="${buildPackages.stdenv.cc}/bin/${buildPackages.stdenv.cc.targetPrefix}cc";
  cxxForBuild="${buildPackages.stdenv.cc}/bin/${buildPackages.stdenv.cc.targetPrefix}c++";
  ccForHost="${stdenv.cc}/bin/${stdenv.cc.targetPrefix}cc";
  cxxForHost="${stdenv.cc}/bin/${stdenv.cc.targetPrefix}c++";
  releaseDir = "target/${stdenv.hostPlatform.config}/${buildType}";

  buildPhaseFn = cargoBuildFlags: with builtins; args.buildPhase or ''
    runHook preBuild
    (
    set -x
    env \
      "CC_${stdenv.buildPlatform.config}"="${ccForBuild}" \
      "CXX_${stdenv.buildPlatform.config}"="${cxxForBuild}" \
      "CC_${stdenv.hostPlatform.config}"="${ccForHost}" \
      "CXX_${stdenv.hostPlatform.config}"="${cxxForHost}" \
      cargo build \
        ${lib.optionalString (buildType == "release") "--release"} --target ${stdenv.hostPlatform.config} \
        --frozen ${concatStringsSep " " cargoBuildFlags}
    )
    # rename the output dir to a architecture independent one
    mapfile -t targets < <(find "$NIX_BUILD_TOP" -type d | grep '${releaseDir}$')
    for target in "''${targets[@]}"; do
      rm -rf "$target/../../${buildType}"
      ln -srf "$target" "$target/../../"
    done
    runHook postBuild
  '';

  commitHash = commit: ''
    GITPATH=$(echo $PATH | tr ':' "\n" | grep git)
    cat << EOF > git
    #!${stdenv.shell}
    case "\$*" in
      "rev-parse --short HEAD") echo -n "${commit}" | cut -c -7 | tr -d "\n";;
      "rev-parse --abbrev-ref HEAD") echo -n "nix-build" ;;
      "diff --quiet --exit-code HEAD") true ;;
      *) ''${GITPATH}/git "\$@" ;;
    esac;
    EOF
    chmod +x git
    export PATH="$(echo $PATH | tr ':' "\n" | grep -v git | tr "\n" ":")$(pwd)"
  '';

  makeJormungandr = { version, rev ? "v${version}", sha256, cargoSha256, name ? "jormungandr-${version}" }:
    rustPlatform.buildRustPackage rec {
      inherit name version buildType;
      src = fetchSrc {
        name = "${name}-source";
        inherit rev sha256;
      };
      inherit cargoSha256;
      nativeBuildInputs = [ pkgconfig ];
      buildInputs = [ sqlite protobuf openssl ]
        ++ lib.optional stdenv.isDarwin Security
        ++ lib.optional stdenv.isLinux systemd;
      preBuild = ''
        ${commitHash rev}
        echo -e "\n[profile.dev]" >> Cargo.toml
        echo "opt-level = 3" >> Cargo.toml
        cd jormungandr
      '';
      buildPhase = buildPhaseFn cargoBuildFlags;
      preInstall = "cd ..";
      cargoBuildFlags = ["--features \"gelf"] ++ lib.optional stdenv.isLinux "systemd" ++ ["\""];
      PROTOC = "${protobuf}/bin/protoc";
      # workaround https://github.com/NixOS/nixpkgs/issues/61618
      preConfigure = ''
        export HOME=`mktemp -d`
      '';
      passthru = { inherit src; };
    };

  makeJcli = { version, rev ? "v${version}", sha256, cargoSha256, name ? "jormungandr-cli-${version}" }:
    rustPlatform.buildRustPackage rec {
      inherit name version buildType;
      src = fetchSrc {
        name = "${name}-source";
        inherit rev sha256;
      };

      inherit cargoSha256;
      nativeBuildInputs = [ pkgconfig ];
      buildInputs = [ sqlite protobuf openssl ]
        ++ lib.optional stdenv.isDarwin Security
        ++ lib.optional stdenv.isLinux systemd;
      preBuild = ''
        ${commitHash rev}
        echo -e "\n[profile.dev]" >> Cargo.toml
        echo "opt-level = 0" >> Cargo.toml
        cd jcli
      '';
      buildPhase = buildPhaseFn cargoBuildFlags;
      preInstall = "cd ..";
      postInstall = ''
        mkdir $out/scripts
        cp scripts/* $out/scripts/
      '';
      cargoBuildFlags = [];
      PROTOC = "${protobuf}/bin/protoc";
      # workaround https://github.com/NixOS/nixpkgs/issues/61618
      preConfigure = ''
        export HOME=`mktemp -d`
      '';
      passthru = { inherit src; };
    };

in {
  inherit makeJormungandr makeJcli;
}
