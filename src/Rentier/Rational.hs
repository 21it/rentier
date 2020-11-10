{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Rentier.Rational (showRational, readRational) where

import           Data.Ratio
import           Data.Text
import           Data.Text.Read
import           Yesod.Core.Dispatch

displayRational :: Int -> Rational -> Text
displayRational len rat =
  pack $ (if num < 0 then "-" else "") ++ shows d ("." ++ right)
  where
    right = case Prelude.take len (go next) of
      ""       -> "0"
      nonEmpty -> nonEmpty
    (d, next) = abs num `quotRem` den
    num = numerator rat
    den = denominator rat

    go 0 = ""
    go x = let (d', next') = (10 * x) `quotRem` den
           in shows d' (go next')

showRational :: Rational -> Text
showRational = displayRational 2

readRational :: Text -> Maybe Rational
readRational raw =
  case rational $ strip raw of
    Right (success, "") ->
      case rational $ showRational success :: Either String (Rational, Text) of
        Right (reparsed, "") ->
          if reparsed == success
            then Just success
            else Nothing
        Right (_, _)  ->
          Nothing
        Left _        ->
          Nothing
    Right (_, _)        ->
      Nothing
    Left _              ->
      Nothing

instance PathPiece Rational where
  fromPathPiece :: Text -> Maybe Rational
  fromPathPiece = readRational

  toPathPiece :: Rational -> Text
  toPathPiece = displayRational 8
