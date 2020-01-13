{ stdenv
, lib
, fetchFromGitHub
, fetchurl
, mono
, pkgconfig
, runCommand
, buildEnv
}:

let
  nupkgSpecs = builtins.fromJSON (builtins.readFile ./nugetpkgs-mono.json);

  allDeps = let
    fetch = n: v:
    let
      raw_source = fetchurl {
        inherit (v) sha256;
        url = "https://www.nuget.org/api/v2/package/${n}/${v.ver}";
      };
    in runCommand "${n}-${v.ver}" {} ''
      mkdir $out
      ln -sv ${raw_source} $out/${n}.${v.ver}.nupkg
    '';
    nupkgs = lib.mapAttrsFlatten (n: v: fetch n v) nupkgSpecs;
  in buildEnv {
    name = "allDeps";
    paths = nupkgs;
  };

  installNupkgs = n: v: "mono '../../lib/NuGet/NuGet.exe' install '${n}' -Version '${v.ver}'";
  installAllNupkgs = lib.concatStringsSep "\n" (lib.mapAttrsFlatten installNupkgs nupkgSpecs);

in stdenv.mkDerivation rec {
  name = "Choco";
  version = "0.10.15";
  src = fetchFromGitHub {
    owner = "chocolatey";
    repo = "choco";
    rev = version;
    sha256 = "0lj1jwwmh3ivvyfia3s15qyp5pad3s16yc7msggpdhipdqji15z4";
  };

  NuGetCachePath = allDeps;

  buildInputs = [ mono pkgconfig ];
  patches = [
    ./uppercut.patch
    ./nunit-tester.patch
  ];

  xbuild = "${mono}/bin/xbuild";
  postPatch = ''
    substituteAll .uppercut .uppercut
    ls -la /
  '';
  buildPhase = ''
    echo Here is now what\'s in the cache...
    ls -lhL $NuGetCachePath
    pushd src/packages
    ${installAllNupkgs}
    popd
    mono --runtime=v4.0.30319 ./lib/NAnt/NAnt.exe /logger:"NAnt.Core.DefaultLogger" /nologo /quiet /f:"/build/source/.build/default.build" /D:build.config.settings="/build/source/.uppercut" /D:microsoft.framework="mono-4.0" /D:run.ilmerge="false" /D:run.nuget="false" $*
  '';
  installPhase = ''
    mkdir -p $out/bin
    ls -la ./code_drop/chocolatey/console/
    cp ./code_drop/chocolatey/console/* $out/bin/
  '';
  doCheck = true;

  passthru = {
    inherit allDeps mono;
  };
  meta = with lib; {
    description = "Chocolatey choco package manager";
    homepage = https://chocolatey.org;
    license = licenses.asl20;
  };
}
