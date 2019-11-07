-- | Build arguments for the CI build program.

module BuildArgs
  ( BuildArgs (BuildArgs, options, command)
  , RebuildOpts (RebuildOpts, optBuildDirectory, optCacheDirectory, optDryRun)
  , Command (Build, CleanupCache, PurgeCache)
    -- * Command line parsing
  , parseArgs
  )
where

import Prelude hiding
    ( FilePath )

import qualified Filesystem.Path.CurrentOS as FP
import Options.Applicative
    ( Parser
    , execParser
    , flag
    , fullDesc
    , help
    , helper
    , idm
    , info
    , long
    , metavar
    , option
    , optional
    , progDesc
    , str
    , subparser
    , (<**>)
    , (<|>)
    )
import qualified Options.Applicative as Opts

import CommonBuild
    ( DryRun (DryRun, Run), FilePath )


data BuildArgs = BuildArgs
    { options :: RebuildOpts
    , command :: Command
    } deriving (Show)

data RebuildOpts = RebuildOpts
    { optBuildDirectory :: Maybe FilePath
    , optCacheDirectory :: Maybe FilePath
    , optDryRun :: DryRun
    } deriving (Show)

data Command = Build | CleanupCache | PurgeCache deriving (Show)

--------------------------------------------------------------------------------
-- Command line parsing

parseArgs :: String -> IO BuildArgs
parseArgs programDescription = execParser opts
  where
    opts = info
        (cmdOpts <**> helper)
        (fullDesc <> progDesc programDescription)
    cmdOpts = BuildArgs
        <$> rebuildOpts
        <*> (cmd <|> pure Build)
    cmd = subparser
        (  Opts.command "build" (info (pure Build) idm)
        <> Opts.command "cleanup-cache" (info (pure CleanupCache) idm)
        <> Opts.command "purge-cache" (info (pure PurgeCache) idm)
        )

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
