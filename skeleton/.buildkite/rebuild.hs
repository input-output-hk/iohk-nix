{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception
import           Control.Monad.Trans.Maybe
import qualified Data.Text                 as T
import           Safe
import           System.Exit               (exitWith)
import           Turtle

data BuildkiteEnv = BuildkiteEnv
  { bkBuildNum   :: Int
  -- ^ The Buildkite build number.
  , bkPipeline   :: Text
  -- ^ The pipeline slug on Buildkite as used in URLs.
  , bkBranch     :: Text
  -- ^ The branch being built.
  , bkBaseBranch :: Text
  -- ^ The base branch that the pull request is targeting, or "" if not a pull
  -- request.
  } deriving (Show)

main :: IO ()
main = do
  bk          <- getBuildkiteEnv
  buildResult <- buildStep
  exitWith buildResult

buildStep :: IO ExitCode
buildStep = do
  echo "+++ Build and test"
  build .&&. test
 where
  cfg = ["--dump-logs", "--color", "always"]
  stackBuild args = run "stack" $ cfg ++ ["build", "--fast"] ++ args
  buildArgs =
    [ "--bench"
    , "--no-run-benchmarks"
    , "--haddock"
    , "--haddock-internal"
    , "--no-haddock-deps"
    ]
  buildAndTest = stackBuild $ ["--tests"] ++ buildArgs
  build        = stackBuild $ ["--no-run-tests"] ++ buildArgs
  test         = stackBuild ["--test", "--jobs", "1"]

getBuildkiteEnv :: IO (Maybe BuildkiteEnv)
getBuildkiteEnv = runMaybeT $ do
  bkBuildNum   <- MaybeT $ needRead "BUILDKITE_BUILD_NUMBER"
  bkPipeline   <- MaybeT $ need "BUILDKITE_PIPELINE_SLUG"
  bkBranch     <- MaybeT $ need "BUILDKITE_BRANCH"
  bkBaseBranch <- MaybeT $ need "BUILDKITE_PULL_REQUEST_BASE_BRANCH"
  pure BuildkiteEnv {..}

needRead :: Read a => Text -> IO (Maybe a)
needRead v = (>>= readMay) . fmap T.unpack <$> need v

run :: Text -> [Text] -> IO ExitCode
run cmd args = do
  printf (s % " " % s % "\n") cmd (T.unwords args)
  res <- proc cmd args empty
  case res of
    ExitSuccess      -> pure ()
    ExitFailure code -> eprintf
      ("error: Command exited with code " % d % "!\nContinuing...\n")
      code
  pure res
