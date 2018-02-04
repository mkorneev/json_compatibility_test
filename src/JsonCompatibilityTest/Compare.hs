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
diffJson (Object left) (Object right) spec =
  case objDiff of
    Right list -> Right $ ObjectDiff list
    Left err   -> Left err
  where
    leftKeys = sortBy (compare `on` fst) $ H.toList left
    rightKeys = sortBy (compare `on` fst) $ H.toList right
    objDiff = sequence $ diffSortedPairs leftKeys rightKeys spec
diffJson (Array a) (Array b) (ArraySpec (Just indexField) compareBy) =
  Right $ ArrayDiff $ diffLists sortedA sortedB
  where
    extractField v =
      case v of
        Object obj -> fromMaybe Null $ H.lookup indexField obj
        _ -> Null
    sortedA = sortBy (\v1 v2 -> compareValues (extractField v1) (extractField v2)) $ V.toList a
    sortedB = sortBy (\v1 v2 -> compareValues (extractField v1) (extractField v2)) $ V.toList b
diffJson (Array a) (Array b) _ = Right $ ArrayDiff $ diffLists (V.toList a) (V.toList b)
diffJson a b spec =
  case isEqual spec a b of
    Right Equal     -> Right $ SameValue a
    Right Similar   -> Right $ SimilarValue a b
    Right Different -> Right $ ChangedValue a b
    Left err    -> Left err


compareValues :: Value -> Value -> Ordering
compareValues (String a) (String b) = compare a b
compareValues (Number a) (Number b) = compare a b
compareValues _ _ = EQ


diffLists :: [Value] -> [Value] -> [ListValueDiff]
diffLists [] [] = []
diffLists [] (added:rest) = AddedItem added : diffLists [] rest
diffLists (removed:rest) [] = RemovedItem removed : diffLists rest []
diffLists (v1:rest1) (v2:rest2)
  | v1 == v2 = SameItem v1 : diffLists rest1 rest2
  | otherwise = RemovedItem v1 : diffLists rest1 (v2 : rest2)


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
