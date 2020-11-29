{-# LANGUAGE OverloadedStrings #-}

module Scheme.Types where

import Data.Array (Array, elems)
import qualified Data.ByteString as BS
import Data.Complex (Complex, imagPart, realPart)
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import qualified Data.Text as T

data Number
  = Integer Integer
  | Real Double
  | Rational Rational
  | Complex (Complex Double)

instance Show Number where
  show (Integer i) = show i
  show (Real d) = show d
  show (Rational r) = show (numerator r) <> "/" <> show (denominator r)
  show (Complex p) = show (realPart p) <> "+" <> show (imagPart p) <> "i"

data SchemeVal
  = List [SchemeVal]
  | PairList [SchemeVal] SchemeVal
  | Vector (Array Int SchemeVal)
  | Bytevector BS.ByteString
  | String Text
  | Character Char
  | Symbol Text
  | Boolean Bool
  | Number Number

instance Show SchemeVal where
  show (List contents) = "(" <> unwords (map show contents) <> ")"
  show (PairList contents cdr) = "(" <> unwords (map show contents) <> " . " <> show cdr <> ")"
  show (Vector vec) = "#(" <> unwords (map show $ elems vec) <> ")"
  show (Bytevector vec) = "#u8(" <> unwords (map show $ BS.unpack vec) <> ")"
  show (String s) = show s
  show (Character a) = show a
  show (Symbol s) = T.unpack s
  show (Boolean True) = "#t"
  show (Boolean False) = "#f"
  show (Number num) = show num
