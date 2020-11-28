module Scheme.Types where

import Data.Complex (Complex, imagPart, realPart)
import Data.Ratio (denominator, numerator)
import Data.Text (Text)

data SchemeVal
  = String Text
  | Character Char
  | Symbol Text
  | Integer Integer
  | Real Double
  | Rational Rational
  | Complex (Complex Double)
  | Boolean Bool

instance Show SchemeVal where
  show (String s) = show s
  show (Character a) = show a
  show (Symbol s) = "'" <> show s
  show (Integer i) = show i
  show (Real d) = show d
  show (Rational r) = show (numerator r) <> "/" <> show (denominator r)
  show (Complex p) = show (realPart p) <> "+" <> show (imagPart p) <> "i"
  show (Boolean True) = "#t"
  show (Boolean False) = "#f"
