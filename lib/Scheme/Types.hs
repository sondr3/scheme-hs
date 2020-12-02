module Scheme.Types where

import Data.Array (Array, elems)
import qualified Data.ByteString as BS
import Data.Complex (Complex ((:+)), imagPart, realPart)
import qualified Data.Map.Strict as Map
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pretty.Simple (pPrint, pPrintLightBg)

type Environment = Map.Map Text SchemeVal

data Number
  = Integer Integer
  | Real Double
  | Rational Rational
  | Complex (Complex Double)
  deriving (Show)

instance Num Number where
  (+) (Integer x) (Integer y) = Integer (x + y)
  (+) (Integer x) (Real y) = Real (fromInteger x + y)
  (+) (Real x) (Integer y) = Real (x + fromInteger y)
  (+) (Integer x) (Rational y) = Rational (fromInteger x + y)
  (+) (Rational x) (Integer y) = Rational (x + fromInteger y)
  (+) (Integer x) (Complex y) = Complex (fromInteger x + y)
  (+) (Complex x) (Integer y) = Complex (x + fromInteger y)
  (+) (Real x) (Real y) = Real (x + y)
  (+) (Real x) (Rational y) = Rational (toRational x + y)
  (+) (Rational x) (Real y) = Rational (x + toRational y)
  (+) (Real x) (Complex y) = Complex ((x :+ 0) + y)
  (+) (Complex x) (Real y) = Complex (x + (y :+ 0))
  (+) (Rational x) (Rational y) = Rational (x + y)
  (+) (Rational x) (Complex y) = Complex (x + y)
  (+) (Complex x) (Rational y) = Complex (x + y)
  (+) (Complex x) (Complex y) = Complex (x + y)
  (*) (Integer x) (Integer y) = Integer (x * y)
  (*) (Real x) (Real y) = Real (x + y)
  (*) (Rational x) (Rational y) = Rational (x + y)
  (*) (Complex x) (Complex y) = Complex (x + y)
  (*) _ _ = error "undefined operation"
  (-) (Integer x) (Integer y) = Integer (x - y)
  (-) (Real x) (Real y) = Real (x + y)
  (-) (Rational x) (Rational y) = Rational (x + y)
  (-) (Complex x) (Complex y) = Complex (x + y)
  (-) _ _ = error "undefined operation"
  abs (Integer x) = Integer (abs x)
  abs (Real x) = Real (abs x)
  abs (Rational x) = Rational (abs x)
  abs _ = error "undefined operation"
  signum (Integer x) = Integer (signum x)
  signum (Real x) = Real (signum x)
  signum (Rational x) = Rational (signum x)
  signum _ = error "undefined operation"
  fromInteger x = Integer x

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
  | Procedure Environment SchemeVal [SchemeVal]
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
showVal Procedure {} = "<λ>"

dumpAST :: SchemeVal -> IO ()
dumpAST = dumpAST' True

dumpAST' :: Bool -> SchemeVal -> IO ()
dumpAST' True = pPrint
dumpAST' False = pPrintLightBg
