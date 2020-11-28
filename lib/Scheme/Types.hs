module Scheme.Types where

import Data.Text (Text)

data SchemeType
  = String Text
  | Character Char
  | Symbol Text
  | Integer Integer
  | Boolean Bool

instance Show SchemeType where
  show (String s) = show s
  show (Character a) = show a
  show (Symbol s) = "'" <> show s
  show (Integer i) = show i
  show (Boolean b) = show b
