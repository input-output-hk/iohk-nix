{ lib
, runCommand
}:
let
  inherit (builtins)
    readDir readFile listToAttrs concatMap match head elemAt fetchGit
  ;
  inherit (lib)
    optional splitString nameValuePair foldr take drop hasPrefix
  ;

  rawCabalProject = origSrcDir:
    if ((readDir origSrcDir)."cabal.project" or "") == "regular"
      then readFile (origSrcDir + "/cabal.project")
      else null;

  parseBlockLines = blockLines: listToAttrs (concatMap (s:
    let pair = match " *([^:]*): *(.*)" s;
    in optional (pair != null) (nameValuePair
          (head pair)
          (elemAt pair 1))) blockLines);

  span = pred: list:
    let n = foldr (x: acc: if pred x then acc + 1 else 0) 0 list;
    in { fst = take n list; snd = drop n list; };

  parseBlock = block:
    let
      x = span (hasPrefix " ") (splitString "\n" block);
    in
      parseBlockLines x.fst;

  parseRepos = projectFile:
    let
      blocks = splitString "\nsource-repository-package\n" projectFile;
    in
      map parseBlock (drop 1 blocks);

  fetchOnMaster = repo:
    fetchGit {
      url = repo.location;
      rev = repo.tag;
      ref = "master";
    };

in
  { src }:
  let
      projectFile = rawCabalProject src;
      repos = parseRepos projectFile;
  in
    runCommand "check-tag-on-master-branch"
      (listToAttrs (map (r:
        nameValuePair "${r.location}" (fetchOnMaster r)
      ) repos))
      ''
        mkdir -p $out
      ''
