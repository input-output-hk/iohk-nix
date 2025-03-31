# Nixpkgs use of release 0.2 results in core dump from buffer overflow:
# See: https://github.com/hogliux/bomutils/issues/33
#
# This PR may be needed if other issues continue:
# https://github.com/hogliux/bomutils/pull/36
final: prev: {
  bomutils = prev.bomutils.overrideAttrs (_: rec {
    version = "master-${builtins.substring 0 7 src.rev}";
    src = prev.fetchFromGitHub {
      owner = "hogliux";
      repo = "bomutils";
      rev = "c247002e2f49878bab32dd0fcf805810ae0992cb";
      sha256 = "sha256-CtRDkx/BmlkIQL0qz6yNgdkM2FZWYSTB7lTRmM6zmXE=";
    };
  });
}
