module Scheme.Types where

import Data.Text (Text)

data SchemeVal
  = String Text
  | Character Char
  | Symbol Text
  | Integer Integer
  | Boolean Bool

instance Show SchemeVal where
  show (String s) = show s
  show (Character a) = show a
  show (Symbol s) = "'" <> show s
  show (Integer i) = show i
  show (Boolean True) = "#t"
  show (Boolean False) = "#f"
