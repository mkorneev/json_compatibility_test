{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Control.Monad     as Monad
import           Data.Aeson
import           JsonCompatibilityTest.Compare
import           JsonCompatibilityTest.Model
import           JsonCompatibilityTest.Spec
import           Test.HUnit
import           Text.RawString.QQ

tests = TestList [
  TestCase $ assertEqual "diff objects"
      (Just $ Right $ ObjectDiff [
        SameKey "a" (SameValue (Number 1.0)),
        SameKey "b" (ChangedValue (Number 1.0) (Number 2.0)),
        AllowedAddedKey "e" (String "2"),
        SameKey "f" (SameValue (Number 1.0)),
        RemovedKey "g" (String "1")])
      (liftM2 (\a b -> diffJson a b Exact)
          (decode [r|{"b": 1, "a": 1, "g": "1", "f": 1}|])
          (decode [r|{"b": 2, "a": 1, "e": "2", "f": 1}|]))

  , TestCase $ assertEqual "diff arrays"
      (Just $ Right $ ObjectDiff [SameKey "a" (ArrayDiff [SameItem (Number 1.0),
                                                  RemovedItem (Number 2.0),
                                                  RemovedItem (Number 3.0),
                                                  AddedItem (Number 2.1),
                                                  AddedItem (Number 4.0)])])
      (liftM2 (\a b -> diffJson a b Exact)
          (decode [r|{"a": [1, 2, 3]}|])
          (decode [r|{"a": [1, 2.1, 4]}|]))

  , TestCase $ assertEqual "merge lists 1"
      ["A", "B", "D"]
      (mergeLists ["A", "D"] ["B", "D"])

  , TestCase $ assertEqual "merge lists 2"
      ["B", "D", "A"]
      (mergeLists ["B", "D"] ["D", "B", "A"])

  , TestCase $ assertEqual "merge lists 3"
      ["E", "A", "B", "C"]
      (mergeLists ["A", "B", "C"] ["E", "C", "B", "A"])

  ]

main :: IO Counts
main = runTestTT tests
