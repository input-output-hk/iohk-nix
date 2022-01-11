{ stdenv
, lib
, python37
, git
, nix-prefetch-git
}:
stdenv.mkDerivation {
  name = "sha256map-regenerate";
  src = ./sha256map-regenerate.py;
  dontUnpack = true;
  dontConfigure = true;
  dontBuild = true;
  installPhase = ''
    bin="$out/bin/$name"
    mkdir -p $out/bin
    sed \
      -e '1c #!${python37}/bin/python' \
      -e '2i import os; os.environ["PATH"] += os.pathsep + os.pathsep.join([${lib.concatMapStringsSep ", " (p: "\"${p}/bin\"") [ git nix-prefetch-git ]}])' \
      < "$src" \
      > "$bin"
    chmod 755 "$bin"
    patchShebangs "$bin"
  '';
}
