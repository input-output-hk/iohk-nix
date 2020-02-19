{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Build where

import Data.Text
    ( Text )

import CommonBuild
    ( BuildkiteEnv
    , CoverallsConfig
    , DryRun
    , ExitCode (ExitSuccess)
    , cacheGetStep
    , cachePutStep
    , cleanBuildDirectory
    , cleanupCacheStep
    , doMaybe
    , echo
    , getBuildkiteEnv
    , getCacheConfig
    , isBorsBuild
    , purgeCacheStep
    , run
    , setupBuildDirectory
    , timeout
    , uploadCoverageStep
    , void
    , weederStep
    , when
    , whenRun
    , (.&&.)
    )

import BuildArgs
    ( BuildArgs (BuildArgs, command, options)
    , Command (Build, CleanupCache, PurgeCache)
    , RebuildOpts (RebuildOpts, optBuildDirectory, optCacheDirectory, optDryRun)
    , parseArgs
    )

import Data.Maybe
    ( fromMaybe )
import System.Exit
    ( exitWith )


newtype Timeout = Timeout { unTimeout :: Int }
  deriving (Eq, Show)

-- | Run the CI build step.
--
-- The specific command and its options can be passed as command line
-- arguments. See module 'BuildArgs'.
doBuild
  :: LibraryName
  -> Optimizations
  -> ShouldUploadCoverage
  -> [TestRun]
  -> CoverallsConfig
  -> Timeout
  -> IO ()
doBuild (LibraryName whichLibrary)
        optimizations
        (ShouldUploadCoverage shouldUploadCoverage)
        testRuns
        coverallsConfig
        testsTimeout                               = do
  BuildArgs {options, command} <-
    parseArgs $ "Build " ++ whichLibrary ++ " with stack in Buildkite"
  let RebuildOpts { optBuildDirectory, optCacheDirectory, optDryRun } = options
  optBuildkiteEnv <- getBuildkiteEnv
  cacheConfig <- getCacheConfig optBuildkiteEnv optCacheDirectory
  case command of
    Build -> do
      doMaybe (setupBuildDirectory optDryRun) optBuildDirectory
      whenRun optDryRun $ do
        cacheGetStep cacheConfig
        cleanBuildDirectory (fromMaybe "." optBuildDirectory)
      buildResult <- buildStep optDryRun optimizations optBuildkiteEnv testRuns testsTimeout
      when (shouldUploadCoverage optBuildkiteEnv) $
        uploadCoverageStep coverallsConfig optDryRun
      whenRun optDryRun $ cachePutStep cacheConfig
      void $ weederStep optDryRun
      exitWith buildResult
    CleanupCache ->
      cleanupCacheStep optDryRun cacheConfig optBuildDirectory
    PurgeCache ->
      purgeCacheStep optDryRun cacheConfig optBuildDirectory

newtype LibraryName = LibraryName String deriving Show

data Optimizations = Standard | Fast deriving (Show, Eq)

-- | Function to determine whether coverage information should be uploaded.
-- This might depend in the Buildkite environment.
newtype ShouldUploadCoverage = ShouldUploadCoverage (Maybe BuildkiteEnv -> Bool)

uploadCoverageIfBors :: ShouldUploadCoverage
uploadCoverageIfBors = ShouldUploadCoverage $ maybe False isBorsBuild

newtype TestRun = TestRun
  { stackTestArgs :: StackExtraTestArgs
  }

-- | Extra arguments for @stack test@
newtype StackExtraTestArgs = StackExtraTestArgs (Maybe BuildkiteEnv -> [Text])

buildStep
  :: DryRun
  -> Optimizations
  -> Maybe BuildkiteEnv
  -> [TestRun]
  -- ^ Used to specify different arguments for different test runs.
  -> Timeout
  -- ^ Test timeout.
  -> IO ExitCode
buildStep dryRun optimizations optBuildkiteEnv testRuns (Timeout testTimeout)=
  echo "--- Build LTS Snapshot"
    *> build Standard ["--only-snapshot"]  .&&.
  echo "--- Build dependencies"
    *> build Standard ["--only-dependencies"] .&&.
  echo "+++ Build"
    *> build optimizations ["--test", "--no-run-tests"] .&&.
  echo "+++ Test"
    *> foldr (.&&.) (pure ExitSuccess) (timeout testTimeout . test <$> testRuns)
  where
  build optimizations' args =
    run dryRun "stack" $ concat
      [ color "always"
      , [ "build" ]
      , [ "--pedantic" ]
      , [ "--bench" ]
      , [ "--no-run-benchmarks" ]
      , [ "--coverage" ]
      , shouldUseFast optimizations'
      , args
      ]

  test (TestRun (StackExtraTestArgs mkTestArgs))
    = run dryRun "stack"
    $ concat
      [ color "always"
      , [ "test" ]
      , [ "--pedantic" ]
      , [ "--coverage" ]
      , shouldUseFast optimizations
      , mkTestArgs optBuildkiteEnv
      ]

  color arg = ["--color", arg]
  shouldUseFast arg = case arg of Standard -> []; Fast -> ["--fast"]
