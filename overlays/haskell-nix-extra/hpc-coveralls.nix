{ mkDerivation, aeson, async, base, bytestring, Cabal, cmdargs
, containers, curl, directory, directory-tree, fetchgit, hpc, HUnit
, process, pureMD5, regex-posix, retry, safe, split, stdenv
, transformers
}:
mkDerivation {
  pname = "hpc-coveralls";
  version = "1.1.0";
  src = fetchgit {
    url = "https://github.com/sevanspowell/hpc-coveralls.git";
    sha256 = "0qw5x7bhcmma90yp97m1y6si8idapxlbmy62rcnnni2avyi1aski";
    rev = "c73e59fbd39ad20e3ab0ad050533496ec8512dbe";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring Cabal cmdargs containers curl directory
    directory-tree hpc process pureMD5 retry safe split transformers
  ];
  executableHaskellDepends = [
    aeson async base bytestring Cabal cmdargs containers curl directory
    directory-tree hpc process pureMD5 regex-posix retry safe split
    transformers
  ];
  testHaskellDepends = [ base HUnit ];
  jailbreak = true;
  homepage = "https://github.com/guillaume-nargeot/hpc-coveralls";
  description = "Coveralls.io support for Haskell.";
  license = stdenv.lib.licenses.bsd3;
}
