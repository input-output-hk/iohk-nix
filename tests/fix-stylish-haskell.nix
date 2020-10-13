{ writeScriptBin, git, fd, stylish-haskell }:

writeScriptBin "fix-stylish-haskell" ''
    ${git}/bin/git diff > pre-stylish.diff
    ${fd}/bin/fd \
      --extension hs \
      --exclude '*/dist/*' \
      --exclude '*/docs/*' \
      --exec ${stylish-haskell}/bin/stylish-haskell -i {}
    ${git}/bin/git diff > post-stylish.diff
    diff pre-stylish.diff post-stylish.diff > /dev/null
    if [ $? != 0 ]
    then
      echo "Changes by stylish have been made. Please commit them."
    else
      echo "No stylish changes were made."
    fi
    rm pre-stylish.diff post-stylish.diff
    exit
''
