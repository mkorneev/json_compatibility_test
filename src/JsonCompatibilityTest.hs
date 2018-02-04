module JsonCompatibilityTest
  ( diffJsonAsXml
  , module JsonCompatibilityTest.XML
  ) where

import           Data.Aeson
import           Text.XML
import           Data.Bifunctor
import           Data.Either
import           JsonCompatibilityTest.Compare
import           JsonCompatibilityTest.Spec
import           JsonCompatibilityTest.XML

diffJsonAsXml :: Value -> Value -> Value -> (Document, Bool)
diffJsonAsXml left right spec = (toXML result, fromRight False $ second isSimilar result)
  where
    result = parseSpec [] spec >>= diffJson left right
