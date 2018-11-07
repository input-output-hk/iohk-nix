name: pkgs: hspkgs: deps: env: code: let
    lib = pkgs.lib;
    ghc = hspkgs.ghcWithPackages deps;
    flags = lib.optionalString pkgs.stdenv.isDarwin "-liconv";  # https://github.com/NixOS/nixpkgs/issues/46814
    builtBinary = pkgs.runCommand "${name}-binary" { buildInputs = [ ghc ]; } ''
      mkdir -p $out/bin/
      ghc ${pkgs.writeText "${name}.hs" code} ${flags} -o $out/bin/${name}
    '';
  in pkgs.runCommand name env ''
    ${builtBinary}/bin/$name
  ''
