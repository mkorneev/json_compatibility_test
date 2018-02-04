module Main where

import           JsonCompatibilityTest

import           Data.Aeson
import           Test.Tasty
import           Test.Tasty.Golden
import           Text.XML

import           Control.Monad
import           Data.ByteString.Lazy.Char8
import           System.Exit
import           System.FilePath
import           System.FilePath.Glob


main :: IO ()
main = do
  paths <- listTestFiles
  golden_tests <- mapM mkGoldenTest paths
  defaultMain (testGroup "Golden Tests" golden_tests)

listTestFiles :: IO [FilePath]
listTestFiles = globDir1 pat "test/golden"
  where pat = compile "*.left.json"

mkGoldenTest :: FilePath -> IO TestTree
mkGoldenTest leftPath =
  return $ goldenVsStringDiff testName commandBuilder goldenPath $ getDiff leftPath rightPath specPath
  where
    testName = dropExtensions $ takeBaseName leftPath
    goldenPath = replaceExtensions leftPath ".golden.xml"
    commandBuilder ref new = ["colordiff", "-u", ref, new]
    rightPath = replaceExtensions leftPath ".right.json"
    specPath = replaceExtensions leftPath ".spec.json"

getDiff :: FilePath -> FilePath -> FilePath -> IO ByteString
getDiff leftPath rightPath specPath = do
  left <- (decode . pack) <$> Prelude.readFile leftPath
  right <- (decode . pack) <$> Prelude.readFile rightPath
  spec <- (decode . pack) <$> Prelude.readFile specPath
  let result = liftM3 diffJsonAsXml left right spec
  return $
    case result of
      Just (doc, theSame) -> renderLBS def {rsPretty = True} doc
      Nothing -> pack "Failed to calculate the diff"

