let
  # The Haskell generic builder falls back to checking 'isExecutable' if 'isLibrary' 
  # is not provided. Tools like cabal2nix therefore often don't provide 'isLibrary', so
  # we need to mimic that logic here.
  isExecutable = args: args.isExecutable or false;
  isLibrary = args: args.isLibrary or (!(isExecutable args));

in { pkgs, filter }:

with pkgs.lib;

self: super: {
    mkDerivation = args: super.mkDerivation (args // optionalAttrs (filter args.pname) {
      doHaddock = isLibrary args;
      postInstall = optionalString (isLibrary args) ''
        ${args.postInstall or ""}
        mkdir -pv $doc/nix-support
        tar -czvf $doc/${args.pname}-${args.version}-docs.tar.gz -C $doc/share/doc/${args.pname}-${args.version}/html .
        echo "file binary-dist $doc/${args.pname}-${args.version}-docs.tar.gz" >> $doc/nix-support/hydra-build-products
        echo "report ${args.pname}-${args.version}-docs.html $doc/share/doc/${args.pname}-${args.version}/html index.html" >> $doc/nix-support/hydra-build-products
      '';
    });
  }
