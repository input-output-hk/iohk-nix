# Gets the value of an environment variable, with a default if it's
# unset or empty.
env: default:
  let
    result = builtins.getEnv env;
  in if result != ""
     then result
     else default
