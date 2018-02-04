module JsonCompatibilityTest.Model where

import           Data.Aeson (Value)
import           Data.Text  (Text)

data ValueDiff =
  SameValue Value | SimilarValue Value Value | ChangedValue Value Value |
  ArrayDiff [ListValueDiff] | ObjectDiff [KeyValueDiff] deriving (Eq, Show)

data ListValueDiff =
  SameItem Value | SimilarItem Value Value |
  SimilarObject [KeyValueDiff] |
  AddedItem Value | RemovedItem Value |
  AllowedAddedItem Value | AllowedRemovedItem Value
  deriving (Eq, Show)

data KeyValueDiff =
  SameKey Text ValueDiff |
  AddedKey Text Value | RemovedKey Text Value |
  AllowedAddedKey Text Value | AllowedRemovedKey Text Value
  deriving (Eq, Show)
