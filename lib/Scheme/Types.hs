module Scheme.Types where

import Data.Array (Array, elems)
import qualified Data.ByteString as BS
import Data.Complex (Complex, imagPart, realPart)
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pretty.Simple (pPrint, pPrintLightBg)

data Number
  = Integer Integer
  | Real Double
  | Rational Rational
  | Complex (Complex Double)
  deriving (Show)

showNumber :: Number -> String
showNumber (Integer i) = show i
showNumber (Real d) = show d
showNumber (Rational r) = show (numerator r) <> "/" <> show (denominator r)
showNumber (Complex p) = show (realPart p) <> "+" <> show (imagPart p) <> "i"

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
  | Nil
  deriving (Show)

showVal :: SchemeVal -> String
showVal (List (Symbol "quote" : xs)) = "'" <> unwords (map showVal xs)
showVal (List (Symbol "quasiquote" : xs)) = "`" <> unwords (map showVal xs)
showVal (List (Symbol "unquote" : xs)) = "," <> unwords (map showVal xs)
showVal (List (Symbol "unquote-splicing" : xs)) = ",@" <> unwords (map showVal xs)
showVal (List contents) = "(" <> unwords (map showVal contents) <> ")"
showVal (PairList contents cdr) = "(" <> unwords (map showVal contents) <> " . " <> show cdr <> ")"
showVal (Vector vec) = "#(" <> unwords (map show $ elems vec) <> ")"
showVal (Bytevector vec) = "#u8(" <> unwords (map show $ BS.unpack vec) <> ")"
showVal (String s) = show s
showVal (Character a) = "#\\" <> [a]
showVal (Symbol s) = T.unpack s
showVal (Boolean True) = "#t"
showVal (Boolean False) = "#f"
showVal (Number num) = showNumber num
showVal Nil = "nil"

dumpAST :: SchemeVal -> IO ()
dumpAST = dumpAST' True

dumpAST' :: Bool -> SchemeVal -> IO ()
dumpAST' True = pPrint
dumpAST' False = pPrintLightBg
