{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE CPP #-}

-- | Given a binary file on MacOS, rewrite some of the dynamic
-- libraries to relative paths.
--
-- 1. Crawl through the tree of libraries
-- 2. filter only for libraries not core to MacOS
-- 3. copy the library to global relative folder
-- 4. set the new library path for all references
-- 5. test that --help executes on the binary
--
-- This program is originally from the Daedalus installer generator.

module Main where

import           System.Directory (copyFile, getPermissions, setOwnerWritable, setPermissions)
import           Text.Megaparsec (Parsec, eof, manyTill, parse, someTill)
import           Text.Megaparsec.Char (eol, spaceChar)
import           Turtle (procStrict, procs)
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Void (Void)
import           Control.Applicative (many)
import           Data.Maybe (catMaybes)
import           System.Exit (die, exitFailure, exitSuccess)
import           System.Environment (getArgs)

#if MIN_VERSION_megaparsec(7,0,0)
import           Text.Megaparsec (anySingle)
#else
import           Text.Megaparsec.Char (anyChar)
anySingle = anyChar
#endif

-- Rewrite libs paths and bundle them
main :: IO ()
main = do
  (outDir, progs) <- parseArgs
  _res <- chain outDir (map T.pack progs)
  pure ()

parseArgs :: IO (FilePath, [FilePath])
parseArgs = getArgs >>= \case
  (outDir:prog:progs) -> pure (outDir, (prog:progs))
  _ -> die "usage: rewrite-libs OUTDIR PROG [PROGS...]" >> exitFailure

systemLibs :: [Text]
systemLibs = ["libSystem.B.dylib"]

-- dir: final path of the files
-- args: libraries to process
-- returns processed libraries
chain :: FilePath -> [Text] -> IO [Text]
chain dir args@(x:xs) = do
    (_, output) <- procStrict "otool" ["-L", x] mempty
    case (parse parseOTool (T.unpack x) output) of
        Left err -> do
            print err
            return []
        Right files -> do
            -- parse again all libraries pointing to nix store that we haven't processed yet
            let libs = filter (T.isPrefixOf "/nix/store/") files
            filtered <- traverse (patchLib x dir) libs
            chained <- chain dir (xs ++ (filter (\f -> not $ elem f args) $ catMaybes filtered))
            return $ x : chained
chain _ [] = return []


patchLib :: Text -> FilePath -> Text -> IO (Maybe Text)
patchLib source dir lib
    | (filter (\pattern -> T.isSuffixOf pattern lib) systemLibs) /= mempty = do
        -- if it's a system lib, just point to correct folder and be done
        print $ "Patching " <> lib <> " as system in " <> source
        procs "install_name_tool" ["-change", lib, "/usr/lib/" <> (filename lib), (T.pack dir) <> "/" <> (filename source)] mempty
        return Nothing
    | otherwise = do
        -- otherwise, copy it to dist and change where it points
        print $ "Bundling " <> lib <> " in " <> source
        -- substitute store path if they are missing
        procs "nix-store" ["-r", lib] mempty
        procs "install_name_tool" ["-change", lib, "@executable_path/" <> (filename lib), (T.pack dir) <> "/" <> (filename source)] mempty
        let dest = dir <> "/" <> (T.unpack $ filename lib)
        copyFile (T.unpack lib) dest
        permissions <- getPermissions dest
        setPermissions dest $ setOwnerWritable True permissions
        return $ Just lib

filename :: Text -> Text
filename path = last $ T.splitOn "/" path

-- otool parser

type Parser = Parsec Void Text

parseLibLine :: Parser Text
parseLibLine = do
    _ <- many spaceChar
    path <- someTill anySingle spaceChar
    _ <- someTill anySingle eol
    return (T.pack path)


parseOTool :: Parser [Text]
parseOTool = do
    _ <- manyTill anySingle eol
    manyTill parseLibLine eof
