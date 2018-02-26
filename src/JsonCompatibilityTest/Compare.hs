{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module JsonCompatibilityTest.Compare where

import           JsonCompatibilityTest.Model
import           JsonCompatibilityTest.Spec

import           Data.Aeson
import           Data.Text         (Text)

import qualified Data.HashMap.Lazy as H
import qualified Data.Text         as T
import qualified Data.Vector       as V

import           Data.Function     (on)
import           Data.List         (sortBy)
import           Data.Bifunctor
import           Data.Maybe

isSimilar :: ValueDiff -> Bool
isSimilar d =
  case d of
    SameValue _ -> True
    SimilarValue _ _ -> True
    ChangedValue _ _ -> False
    ArrayDiff l -> all sameItem l
    ObjectDiff l -> all samePair l
  where sameItem i =
          case i of
            SameItem _ -> True
            SimilarItem _ _ -> True
            AddedItem _ -> False
            RemovedItem _ -> False
            AllowedAddedItem _ -> True
            AllowedRemovedItem _ -> True
            SimilarObject obj -> isSimilar (ObjectDiff obj)
        samePair p =
          case p of
            AddedKey _ _ -> False
            RemovedKey _ _ -> False
            AllowedAddedKey _ _ -> True
            AllowedRemovedKey _ _ -> True
            SameKey k v -> isSimilar v

diffJson :: Value -> Value -> Spec -> Either String ValueDiff
diffJson left@(Object _) right@(Object _) (ArraySpec _ _ spec) = diffJson left right spec
diffJson (Object left) (Object right) spec =
  case objDiff of
    Right list -> Right $ ObjectDiff list
    Left err   -> Left err
  where
    leftKeys = sortBy (compare `on` fst) $ H.toList left
    rightKeys = sortBy (compare `on` fst) $ H.toList right
    objDiff = sequence $ diffSortedPairs leftKeys rightKeys spec
diffJson (Array a) (Array b) spec@(ArraySpec (Just indexField) compareBy _) =
  second ArrayDiff $ sequence $ diffLists sortedA sortedB spec
  where
    sortedA = sortBy (\v1 v2 -> compareValuesByField indexField v1 v2) $ V.toList a
    sortedB = sortBy (\v1 v2 -> compareValuesByField indexField v1 v2) $ V.toList b
diffJson (Array a) (Array b) spec = second ArrayDiff $ sequence $ diffLists (V.toList a) (V.toList b) spec
diffJson a b spec =
  case isEqual spec a b of
    Right Equal     -> Right $ SameValue a
    Right Similar   -> Right $ SimilarValue a b
    Right Different -> Right $ ChangedValue a b
    Left err        -> Left err


extractField :: Text -> Value -> Value
extractField indexField v =
  case v of
    Object obj -> fromMaybe Null $ H.lookup indexField obj
    _ -> Null

compareValues :: Value -> Value -> Ordering
compareValues (String a) (String b) = compare a b
compareValues (Number a) (Number b) = compare a b
compareValues _ _ = EQ

compareValuesByField :: Text -> Value -> Value -> Ordering
compareValuesByField field v1 v2 = compareValues (extractField field v1) (extractField field v2)


toSimilar (ObjectDiff list) = SimilarObject list
toSimilar _ = undefined

diffLists :: [Value] -> [Value] -> Spec -> [Either String ListValueDiff]
diffLists [] [] _ = []
diffLists [] (added:rest) spec = Right (AddedItem added) : diffLists [] rest spec
diffLists (removed:rest) [] spec = Right (RemovedItem removed) : diffLists rest [] spec
diffLists (v1:rest1) (v2:rest2) spec =
  case isEqual spec v1 v2 of
    Right Equal     -> Right (SameItem v1) : diffLists rest1 rest2 spec
    Right Similar   -> case v1 of
                         Object _ -> second toSimilar (diffJson v1 v2 spec) : diffLists rest1 rest2 spec
                         _ -> Right (SimilarItem v1 v2) : diffLists rest1 rest2 spec
    Right Different ->
      case spec of
        spec@(ArraySpec (Just indexField) _ _) -> case compareValuesByField indexField v1 v2 of
          LT -> Right (RemovedItem v1) : diffLists rest1 (v2 : rest2) spec
          EQ -> Right (RemovedItem v1) : diffLists rest1 (v2 : rest2) spec
          GT -> Right (AddedItem v2) : diffLists (v1 : rest1) rest2 spec
        _ -> Right (RemovedItem v1) : diffLists rest1 (v2 : rest2) spec
    Left err        -> Left err : diffLists rest1 rest2 spec


diffSortedPairs :: [(Text, Value)] -> [(Text, Value)] -> Spec -> [Either String KeyValueDiff]
diffSortedPairs [] [] _ = []
diffSortedPairs [] ((added_key, added_value):rest) spec =
  (Right $ AllowedAddedKey added_key added_value) : diffSortedPairs [] rest spec
diffSortedPairs ((removed_key, removed_value):rest) [] spec =
  (Right $ RemovedKey removed_key removed_value) : diffSortedPairs rest [] spec
diffSortedPairs ((k1, v1):rest1) ((k2, v2):rest2) spec
  | k1 == k2 =
    let subSpec =
          case spec of
            ObjectSpec s -> H.lookupDefault Exact k1 s
            _            -> spec
    in case diffJson v1 v2 subSpec of
         Right value -> (Right $ SameKey k1 value) : diffSortedPairs rest1 rest2 spec
         Left err -> Left err : diffSortedPairs rest1 rest2 spec
  | k1 `elem` map fst rest2 = (Right $ AllowedAddedKey k2 v2) : diffSortedPairs ((k1, v1) : rest1) rest2 spec
  | otherwise = (Right $ RemovedKey k1 v1) : diffSortedPairs rest1 ((k2, v2) : rest2) spec
