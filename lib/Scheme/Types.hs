module Scheme.Types where

import Data.Text (Text)

data SchemeVal
  = String Text
  | Character Char
  | Symbol Text
  | Integer Integer
  | Double Double
  | Boolean Bool

instance Show SchemeVal where
  show (String s) = show s
  show (Character a) = show a
  show (Symbol s) = "'" <> show s
  show (Integer i) = show i
  show (Double d) = show d
  show (Boolean True) = "#t"
  show (Boolean False) = "#f"
