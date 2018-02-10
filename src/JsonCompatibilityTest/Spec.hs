{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module JsonCompatibilityTest.Spec where

import qualified Data.HashMap.Lazy as H
import qualified Data.Vector as V
import           Data.Aeson
import           Data.Time
import           Data.Time.ISO8601
import           Control.Monad
import           Data.List.Split
import           Data.Scientific   (Scientific, toRealFloat)
import           Data.String
import           Data.Text         (Text, unpack)
import           Text.Printf
import           Data.List
import           Control.Applicative
import           Data.Maybe
import           Data.Bifunctor

data Spec
  = Exact
  | IgnoreValue
  | Relative Scientific
  | Absolute Scientific
  | TimeDiff NominalDiffTime
  | ObjectSpec (H.HashMap Text Spec)
  | ArraySpec (Maybe Text) (Maybe Text) Spec

data Equality = Equal | Similar | Different

isEqual :: Spec -> Value -> Value -> Either String Equality
isEqual Exact a b
  | a == b = Right Equal
  | otherwise = Right Different

isEqual IgnoreValue a b
  | a == b = Right Equal
  | otherwise = Right Similar

isEqual (Absolute tolerance) (Number a) (Number b)
  | a == b = Right Equal
  | otherwise = Right $ if abs (a - b) <= tolerance then Similar else Different

isEqual (Absolute tolerance) a b =
  Left $ printf "Absolute tolerance is defined only for numbers. Applied for %s and %s" (show a) (show b)

isEqual (Relative tolerance) (Number a) (Number b)
  | a == b = Right Equal
  | otherwise = Right $
      if abs (toRealFloat a - toRealFloat b) / toRealFloat b <= toRealFloat tolerance then Similar else Different

isEqual (Relative tolerance) a b =
  Left $ printf "Relative tolerance is defined only for numbers. Applied for %s and %s" (show a) (show b)

isEqual (TimeDiff tolerance) (String a) (String b)
  | a == b = Right Equal
  | otherwise = case timeDiff of
                  Just d  -> Right $ if abs d <= tolerance then Similar else Different
                  Nothing -> Left "Cannot parse times"
  where
    timeDiff = liftM2 diffUTCTime (parseISO8601 $ unpack a) (parseISO8601 $ unpack b)

isEqual (TimeDiff tolerance) a b =
  Left $ printf "Time tolerance is defined only for timestamps. Applied for %s and %s" (show a) (show b)

isEqual (ArraySpec indexBy (Just field) _) (Object a) (Object b)
  | a == b = Right Equal
  | extractField a == extractField b = Right Similar
  | otherwise = Right Different
    where
      extractField obj = fromMaybe Null $ H.lookup field obj

isEqual (ArraySpec indexBy Nothing spec) a@(Object _) b@(Object _) = isEqual spec a b
isEqual (ArraySpec indexBy compareBy spec) a b = isEqual spec a b

isEqual (ObjectSpec _) _ _ = Right Similar

type JPath = [Text]
showJPath :: JPath -> String
showJPath = intercalate "/" . fmap unpack

parseSpec :: JPath -> Value -> Either String Spec
parseSpec path (Object spec)
  | Just (Number n) <- H.lookup "@rel" spec = Right $ Relative n
  | Just _ <- H.lookup "@rel" spec = Left "@rel should be a number"
  | Just (Number n) <- H.lookup "@abs" spec = Right $ Absolute n
  | Just _ <- H.lookup "@abs" spec = Left "@abs should be a number"
  | Just (String s) <- H.lookup "@time" spec =
    case splitOn " " (unpack s) of
      [n, "sec"] -> Right $ TimeDiff (fromIntegral $ read n)
      [n, "min"] -> Right $ TimeDiff (fromIntegral $ read n * 60)
      _ -> Left $ printf "Wrong time diff format: %s" s
  | not $ null specKeys = Left $ printf "Unknown spec %s" $ head specKeys
  | otherwise = ObjectSpec <$> sequence (H.mapWithKey (\k v -> parseSpec (path ++ [k]) v) spec)
  where
    specKeys = filter (\k -> head (unpack k) == '@') (H.keys spec)

parseSpec path (Array v)
  | [Object arraySpec, itemSpec] <- V.toList v =
    let indexBy:compareBy:_ = fmap (\k -> H.lookup k arraySpec >>= getText) ["@index_by", "@compare_by"]
    in second (ArraySpec indexBy compareBy) $ parseSpec (path ++ ["[]"]) itemSpec
  | otherwise = Left "Arrays in the spec should contain exactly two objects (array spec and item spec)"
parseSpec path (Number _) = Left "Cannot use numbers as spec values"
parseSpec path (Bool _) = Left "Cannot use bools as spec values"
parseSpec path Null = Left "Cannot use null as spec values"
parseSpec path (String "@ignore_value") = Right IgnoreValue
parseSpec path (String s) = Left $ printf "Cannot recognize spec value '%s' at /%s" s (showJPath path)

getText :: Value -> Maybe Text
getText (String t) = Just t
getText _ = Nothing