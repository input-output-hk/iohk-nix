{ iohkSkeletonPackages ? import ../default.nix {}
, pkgs ? iohkSkeletonPackages.pkgs
, buildTools ? with pkgs; [ git gnumake ]
}:

with pkgs.lib;
with pkgs;

let
  baseTools = [ stack gnused coreutils nix gnutar gzip ];
  deps = ps: [ps.turtle ps.safe ps.transformers];

  stackRebuild = runCommand "stack-rebuild" {} ''
    ${haskellPackages.ghcWithPackages deps}/bin/ghc -o $out ${./rebuild.hs}
  '';

in
  writeScript "stack-rebuild-wrapped" ''
    #!${stdenv.shell}
    export PATH=${lib.makeBinPath (baseTools ++ buildTools)}
    exec ${stackRebuild} "$@"
  ''
