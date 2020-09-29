{ mkDerivation, aeson, async, base, bytestring, Cabal, cmdargs
, containers, curl, directory, directory-tree, fetchgit, hpc, HUnit
, process, pureMD5, regex-posix, retry, safe, split, stdenv
, transformers
}:
mkDerivation {
  pname = "hpc-coveralls";
  version = "1.2.0";
  src = fetchgit {
    url = "https://github.com/sevanspowell/hpc-coveralls.git";
    sha256 = "1bygx1j7jzlxfaq2hvcpd9nqmy3wvpsvfw5l3ly0783fy6bnkj4b";
    rev = "225074ef0ca317a266b3da2986b1ff4c33120ea7";
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
