{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import           JsonCompatibilityTest
import           Data.Aeson
import           Control.Monad
import           System.Console.CmdArgs
import           System.Environment            (getArgs, withArgs)
import           System.Exit
import           Data.Version
import           Paths_json_compatibility_test
import qualified Text.XML                      as XML
import qualified Data.ByteString.Lazy.Char8    as B

differenceFoundExitCode = 1

parseErrorExitCode = 2

data AppOptions = AppOptions
  { oldFile  :: FilePath
  , newFile  :: FilePath
  , specFile :: FilePath
  , outFile  :: FilePath
  } deriving (Data, Typeable, Show, Eq)

appOptions =
  AppOptions
  { oldFile = def &= typ "OLDFILE" &= argPos 0
  , newFile = def &= typ "NEWFILE" &= argPos 1
  , specFile = def &= typ "SPECFILE" &= argPos 2
  , outFile = "diff.xml" &= help "Output file" &= typFile
  } &=
  help "Test if JSON files are backwards compatible" &=
  helpArg [explicit, name "help", name "h"] &=
  program "json-compatibility-test" &=
  summary ("json-compatibility-test v" ++ showVersion version)

main = do
  args <- getArgs
  opts <-
    (if null args
       then withArgs ["--help"]
       else id) $
    cmdArgs appOptions
  oldFile <- (decode . B.pack) <$> Prelude.readFile (oldFile opts)
  newFile <- (decode . B.pack) <$> Prelude.readFile (newFile opts)
  specFile <- (decode . B.pack) <$> Prelude.readFile (specFile opts)
  case liftM3 diffJsonAsXml oldFile newFile specFile of
    Just (doc, theSame) -> do
      XML.writeFile XML.def (outFile opts) doc
      if theSame
        then return exitSuccess
        else return exitFailure differenceFoundExitCode
    Nothing -> return exitFailure parseErrorExitCode
