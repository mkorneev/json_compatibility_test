{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module JsonCompatibilityTest.XML (toXML) where

import           JsonCompatibilityTest.Model

import           Data.Aeson
import           Text.Hamlet.XML
import           Text.RawString.QQ
import           Text.XML

import           Data.HashMap.Strict (toList)
import qualified Data.List           as L
import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Data.Vector         as V

toXML :: Either String ValueDiff -> Document
toXML arg =
  let root = case arg of
               Right v -> renderDiff v
               Left err -> Element "error" M.empty [NodeContent $ T.pack err]
  in Document
       (Prologue [] Nothing [MiscInstruction (Instruction "xml-stylesheet" "type=\"text/xsl\" href=\"diff.xsl\"")])
       root
       []

renderDiff :: ValueDiff -> Element
renderDiff (SameValue v) = elemClassElem "span" "same" (renderValue v)
renderDiff (SimilarValue v1 v2) = elemClassNodes "span" "similar" [
    NodeElement $ elemClassElem "span" "from" (renderValue v1),
    NodeElement $ elemClassElem "span" "to" (renderValue v2)
  ]
renderDiff (ChangedValue v1 v2) = elemClassNodes "span" "changed" [
    NodeElement $ elemClassElem "span" "from" (renderValue v1),
    NodeElement $ elemClassElem "span" "to" (renderValue v2)
  ]

renderDiff (ArrayDiff list) = elemClassNodes "span" "list" [xml|
  <span>[
  <br>
  ^{joinWithComma1 $ map (NodeElement . renderListDiff) list}
  <br>
  <span>]
|]

renderDiff (ObjectDiff pairs) = elemClassNodes "div" "obj" [xml|
  <span>{
  <div class="obj_content">
    ^{joinWithComma $ map renderKeyValueDiff pairs}
  <span>}
|]

joinWithComma :: [[Node]] -> [Node]
joinWithComma = L.intercalate [comma, br]
  where comma = nodeClassText "span" "comma" ","
        br = NodeElement $ Element "br" M.empty []

joinWithComma1 :: [Node] -> [Node]
joinWithComma1 l = joinWithComma $ map (: []) l

renderKeyValueDiff :: KeyValueDiff -> [Node]
renderKeyValueDiff (SameKey k v) = [xml|
    <span class="same_key">
      <span>"
      <span class="key">#{k}
      <span>": #
        ^{[NodeElement $ renderDiff v]}
|]

renderKeyValueDiff (AddedKey k v) = renderKey "added_key" k v
renderKeyValueDiff (RemovedKey k v) = renderKey "removed_key" k v
renderKeyValueDiff (AllowedAddedKey k v) = renderKey "allowed_added_key" k v
renderKeyValueDiff (AllowedRemovedKey k v) = renderKey "allowed_removed_key" k v

renderKey cls k v = [xml|
    <span class="#{cls}">
      <span>"
      <span class="key">#{k}
      <span>": #
        ^{[NodeElement $ renderValue v]}
|]

nodeClassText name cls text = NodeElement $ elemClassText name cls text
elemClassText name cls text = Element name (M.singleton "class" cls) [NodeContent text]

nodeClassElem name cls elem = NodeElement $ elemClassElem name cls elem
elemClassNodes name cls = Element name (M.singleton "class" cls)
elemClassElem name cls elem = Element name (M.singleton "class" cls) [NodeElement elem]


renderListDiff :: ListValueDiff -> Element
renderListDiff (SameItem i) = elemClassElem "span" "same item" $ renderValue i
renderListDiff (SimilarItem a b) = elemClassNodes "span" "similar item" [
                                       NodeElement $ elemClassElem "span" "from" (renderValue a),
                                       NodeElement $ elemClassElem "span" "to" (renderValue b)
                                     ]
renderListDiff (AddedItem i) = elemClassElem "span" "added item"  $ renderValue i
renderListDiff (RemovedItem i) = elemClassElem "span" "removed item"  $renderValue i
renderListDiff (AllowedAddedItem i) = elemClassElem "span" "allowed_added item"  $ renderValue i
renderListDiff (AllowedRemovedItem i) = elemClassElem "span" "allowed_removed item"  $renderValue i
renderListDiff (SimilarObject l) = elemClassElem "span" "similar item" $ renderDiff (ObjectDiff l)

renderValue :: Value -> Element
renderValue Null = elemClassText "span" "null" "null"
renderValue (Bool b) = elemClassText "span" "bool" (T.pack (if b then "true" else "false"))
renderValue (Number n) = elemClassText "span" "num" $ (T.pack . show) n
renderValue (String s) = elemClassText "span" "str" $ (T.pack . show) s
renderValue (Array vector) =
  elemClassNodes "span" "list" $
  [xml|
  <span>[
  <br>
  ^{renderList vector}
  <br>
  <span>]
|]
  where
    renderList = joinWithComma1 . V.toList . V.map (nodeClassElem "span" "item" . renderValue)

renderValue (Object hashMap) = elemClassNodes "span" "obj" $ [xml|
  <span>{
  <br>
  ^{joinWithComma $ map renderPair $ toList hashMap}
  <br>
  <span>}
|]


renderPair :: (T.Text, Value) -> [Node]
renderPair (k, v) = [xml|
   <span class="same_key">
     <span>"
     <span class="key">#{k}
     <span>": #
       ^{[NodeElement $ renderValue v]}
|]
