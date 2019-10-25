{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

import CommonBuild

import Options.Applicative
import Data.Maybe
    ( fromMaybe )
import System.Exit
    ( exitWith )

import qualified Data.Text as T
import qualified Filesystem.Path.CurrentOS as FP

data RebuildOpts = RebuildOpts
    { optBuildDirectory :: Maybe FilePath
    , optCacheDirectory :: Maybe FilePath
    , optDryRun :: DryRun
    } deriving (Show)

data Command = Build | CleanupCache | PurgeCache deriving (Show)

main :: IO ()
main = do
    (RebuildOpts{..}, cmd) <- parseOpts
    bk <- getBuildkiteEnv
    cacheConfig <- getCacheConfig bk optCacheDirectory
    case cmd of
        Build -> do
            doMaybe (setupBuildDirectory optDryRun) optBuildDirectory
            whenRun optDryRun $ do
                cacheGetStep cacheConfig
                cleanBuildDirectory (fromMaybe "." optBuildDirectory)
            buildResult <- buildStep optDryRun bk
            when (shouldUploadCoverage bk) $
              uploadCoverageStep "CARDANO_WALLET_COVERALLS_REPO_TOKEN" optDryRun
            whenRun optDryRun $ cachePutStep cacheConfig
            void $ weederStep optDryRun
            exitWith buildResult
        CleanupCache ->
            cleanupCacheStep optDryRun cacheConfig optBuildDirectory
        PurgeCache ->
            purgeCacheStep optDryRun cacheConfig optBuildDirectory

rebuildOpts :: Parser RebuildOpts
rebuildOpts = RebuildOpts
    <$> optional buildDir
    <*> optional cacheName
    <*> dryRun
  where
    buildDir = option
        (FP.decodeString <$> str)
        (  long "build-dir"
        <> metavar "DIR"
        <> help "Copy sources to directory before building"
        )
    cacheName = option
        (FP.decodeString <$> str)
        (  long "cache-dir"
        <> metavar "DIR"
        <> help "Location of project's cache"
        )
    dryRun = flag Run DryRun
        (  long "dry-run"
        <> help "Print what build commands would be run, without executing them"
        )

parseOpts :: IO (RebuildOpts, Command)
parseOpts = execParser opts
  where
    opts = info
        (cmdOpts <**> helper)
        (fullDesc <> progDesc "Build cardano-wallet with stack in Buildkite")
    cmdOpts = (,)
        <$> rebuildOpts
        <*> (cmd <|> pure Build)
    cmd = subparser
        (  command "build" (info (pure Build) idm)
        <> command "cleanup-cache" (info (pure CleanupCache) idm)
        <> command "purge-cache" (info (pure PurgeCache) idm)
        )

data BuildOpt = Standard | Fast deriving (Show, Eq)

data QA = QuickTest | FullTest deriving (Show, Eq)

data Jobs = Serial | Parallel deriving (Show, Eq)

buildStep :: DryRun -> Maybe BuildkiteEnv -> IO ExitCode
buildStep dryRun bk =
    echo "--- Build LTS Snapshot"
        *> build Standard ["--only-snapshot"]  .&&.
    echo "--- Build dependencies"
        *> build Standard ["--only-dependencies"] .&&.
    echo "+++ Build"
        *> build Fast ["--test", "--no-run-tests"] .&&.
    echo "+++ Test"
        *> timeout 30 (test Fast Serial .&&. test Fast Parallel)
  where
    build opt args =
        run dryRun "stack" $ concat
            [ color "always"
            , [ "build" ]
            , [ "--bench" ]
            , [ "--no-run-benchmarks" ]
            , [ "--haddock" ]
            , [ "--haddock-internal" ]
            , [ "--no-haddock-deps" ]
            , [ "--coverage" ]
            , fast opt
            , args
            ]

    test opt behavior =
        run dryRun "stack" $ concat
            [ color "always"
            , [ "test" ]
            , [ "--coverage" ]
            -- FIXME
            -- Figure out what's going on with http-bridge cluster setup...
            -- maybe...
            , skip "http-bridge-integration"
            , fast opt
            , case qaLevel bk of
                QuickTest -> skip "integration"
                FullTest -> []
            , case behavior of
                Serial ->
                    ta (match serialTests ++ jobs 1) ++ jobs 1
                Parallel ->
                    ta (skip serialTests)
            ]

    color arg = ["--color", arg]
    fast  arg = case arg of Standard -> []; Fast -> ["--fast"]
    jobs  arg = ["--jobs", T.pack (show @Int arg)]
    skip  arg = ["--skip", arg]
    match arg = ["--match", arg]
    ta    arg = ["--ta", T.unwords arg]

    serialTests = "SERIAL"

-- | How much time to spend executing tests.
qaLevel :: Maybe BuildkiteEnv -> QA
qaLevel = maybe QuickTest level
  where
    level bk
        | isBorsBuild bk = FullTest
        | onDefaultBranch bk = QuickTest
        | otherwise = QuickTest

-- | Whether to upload test coverage information to coveralls.io.
shouldUploadCoverage :: Maybe BuildkiteEnv -> Bool
shouldUploadCoverage bk = qaLevel bk == FullTest
