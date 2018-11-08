{ pkgs, filter }:

with pkgs.lib;

self: super: {
    mkDerivation = args: super.mkDerivation (args // optionalAttrs (filter args.pname) {
      doHaddock = true;
      postInstall = ''
        ${args.postInstall or ""}
        mkdir -pv $doc/nix-support
        tar -czvf $doc/${args.pname}-${args.version}-docs.tar.gz -C $doc/share/doc/${args.pname}-${args.version}/html .
        echo "file binary-dist $doc/${args.pname}-${args.version}-docs.tar.gz" >> $doc/nix-support/hydra-build-products
        echo "report ${args.pname}-${args.version}-docs.html $doc/share/doc/${args.pname}-${args.version}/html index.html" >> $doc/nix-support/hydra-build-products
      '';
    });
  }
