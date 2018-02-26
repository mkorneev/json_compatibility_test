{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module JsonCompatibilityTest.XML (toXML) where

import           JsonCompatibilityTest.Model

import           Data.Aeson
import           Text.Hamlet.XML
import           Text.RawString.QQ
import           Text.XML
import           Data.Scientific

import           Data.HashMap.Strict (toList)
import qualified Data.List           as L
import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Data.Vector         as V

toXML :: Either String ValueDiff -> Document
toXML arg =
  let root = case arg of
               Right v -> renderDiff v
               Left err -> e "error" [NodeContent $ T.pack err]
  in Document
       (Prologue [] Nothing [MiscInstruction (Instruction "xml-stylesheet" "type=\"text/xsl\" href=\"https://raw.githubusercontent.com/mkorneev/json_compatibility_test/master/render_json_diff.xsl\"")])
       root
       []

e name = Element name M.empty
el name list = Element name M.empty (map NodeElement list)
ee name elem = el name [elem]

renderDiff :: ValueDiff -> Element
renderDiff (SameValue v) = ee "same" $ renderValue v
renderDiff (SimilarValue v1 v2) = e "similar" $ renderFromTo v1 v2
renderDiff (ChangedValue v1 v2) = e "changed" $ renderFromTo v1 v2
renderDiff (ArrayDiff list) = el "list" $ map renderListDiff list
renderDiff (ObjectDiff pairs) = e "obj_diff" $ concatMap renderKeyValueDiff pairs

renderKeyValueDiff :: KeyValueDiff -> [Node]
renderKeyValueDiff (SameKey k v) = [xml|
    <pair class="same_key">
      <key>#{k}
      <value>
        ^{[NodeElement $ renderDiff v]}
|]

renderKeyValueDiff (AddedKey k v) = renderKey "added_key" k v
renderKeyValueDiff (RemovedKey k v) = renderKey "removed_key" k v
renderKeyValueDiff (AllowedAddedKey k v) = renderKey "allowed_added_key" k v
renderKeyValueDiff (AllowedRemovedKey k v) = renderKey "allowed_removed_key" k v

renderKey cls k v = [xml|
    <pair class="#{cls}">
      <key>#{k}
      <value>^{[NodeElement $ renderValue v]}
|]

elemText name text = e name [NodeContent text]

nodeClassElem name cls elem = NodeElement $ elemClassElem name cls elem
elemClassNodes name cls = Element name (M.singleton "class" cls)
elemClassElem name cls elem = Element name (M.singleton "class" cls) [NodeElement elem]

renderListDiff :: ListValueDiff -> Element
renderListDiff (SameItem i) = elemClassElem "item" "same" $ renderValue i
renderListDiff (SimilarItem a b) = elemClassNodes "item" "similar" $ renderFromTo a b
renderListDiff (AddedItem i) = elemClassElem "item" "added"  $ renderValue i
renderListDiff (RemovedItem i) = elemClassElem "item" "removed"  $renderValue i
renderListDiff (AllowedAddedItem i) = elemClassElem "item" "allowed_added"  $ renderValue i
renderListDiff (AllowedRemovedItem i) = elemClassElem "item" "allowed_removed"  $renderValue i
renderListDiff (SimilarObject l) = elemClassElem "item" "similar" $ renderDiff (ObjectDiff l)

renderValue :: Value -> Element
renderValue Null = e "null" []
renderValue (Bool b) = elemText "bool" (T.pack (if b then "true" else "false"))
renderValue (Number n) = elemText "num" $ T.pack $ formatScientific Fixed Nothing n
renderValue (String s) = elemText "str" s
renderValue (Array vector) = el "list" $ map (ee "item" . renderValue) $ V.toList vector
renderValue (Object hashMap) = e "obj" $ concatMap renderPair (toList hashMap)


renderPair :: (T.Text, Value) -> [Node]
renderPair (k, v) = [xml|
   <pair class="same_key">
     <key>#{k}
     <value>
       ^{[NodeElement $ renderValue v]}
|]

renderFromTo :: Value -> Value -> [Node]
renderFromTo a b = [xml|
    <from>
      ^{[NodeElement $ renderValue a]}
    <to>
      ^{[NodeElement $ renderValue b]}
  |]

