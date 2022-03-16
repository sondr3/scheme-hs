{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Scheme.Types where

import Control.Exception (Exception, throw)
import Control.Monad.Except (ExceptT)
import Data.Array (Array, elems)
import qualified Data.ByteString as BS
import Data.Complex (Complex ((:+)), imagPart, realPart)
import Data.IORef (IORef)
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import System.IO (Handle)
import Text.Pretty.Simple (pPrint, pPrintLightBg)
import Text.Show.Functions ()

type Env = IORef [(Text, IORef SchemeVal)]

type SchemeResult = Either SchemeError

type IOSchemeResult = ExceptT SchemeError IO

extractValue :: Either a p -> p
extractValue (Right val) = val
extractValue _ = error "Called with Left"

data Number
  = Integer Integer
  | Real Double
  | Rational Rational
  | Complex (Complex Double)
  deriving stock (Show, Typeable, Eq)

showNum :: Number -> Text
showNum (Integer i) = T.pack $ show i
showNum (Real d) = T.pack $ show d
showNum (Rational r) = T.pack $ show (numerator r) <> "/" <> show (denominator r)
showNum (Complex p) = T.pack $ show (realPart p) <> "+" <> show (imagPart p) <> "i"

instance Ord Number where
  compare (Integer x) (Integer y) = compare x y
  compare (Integer x) (Real y) = compare (fromInteger x) y
  compare (Integer x) (Rational y) = compare (fromInteger x) y
  compare (Real x) (Integer y) = compare x (fromInteger y)
  compare (Rational x) (Integer y) = compare x (fromInteger y)
  compare (Real x) (Real y) = compare x y
  compare (Real x) (Rational y) = compare (toRational x) y
  compare (Rational x) (Real y) = compare x (toRational y)
  compare (Rational x) (Rational y) = compare x y
  compare _ _ = throw $ InvalidOperation "Cannot compare complex numbers"

instance Num Number where
  fromInteger = Integer

  (Integer x) + (Integer y) = Integer (x + y)
  (Integer x) + (Real y) = Real (fromInteger x + y)
  (Integer x) + (Rational y) = Rational (fromInteger x + y)
  (Integer x) + (Complex y) = Complex (fromInteger x + y)
  (Real x) + (Integer y) = Real (x + fromInteger y)
  (Rational x) + (Integer y) = Rational (x + fromInteger y)
  (Complex x) + (Integer y) = Complex (x + fromInteger y)
  (Real x) + (Real y) = Real (x + y)
  (Real x) + (Rational y) = Rational (toRational x + y)
  (Real x) + (Complex y) = Complex ((x :+ 0.0) + y)
  (Rational x) + (Real y) = Rational (x + toRational y)
  (Complex x) + (Real y) = Complex (x + (y :+ 0.0))
  (Rational x) + (Rational y) = Rational (x + y)
  (Complex x) + (Complex y) = Complex (x + y)
  _ + _ = throw $ InvalidOperation "Cannot add rationals and complex numbers"

  (Integer x) * (Integer y) = Integer (x * y)
  (Integer x) * (Real y) = Real (fromInteger x * y)
  (Integer x) * (Rational y) = Rational (fromInteger x * y)
  (Integer x) * (Complex y) = Complex (fromInteger x * y)
  (Real x) * (Integer y) = Real (x * fromInteger y)
  (Rational x) * (Integer y) = Rational (x * fromInteger y)
  (Complex x) * (Integer y) = Complex (x * fromInteger y)
  (Real x) * (Real y) = Real (x * y)
  (Real x) * (Rational y) = Rational (toRational x * y)
  (Real x) * (Complex y) = Complex ((x :+ 0.0) * y)
  (Rational x) * (Real y) = Rational (x * toRational y)
  (Complex x) * (Real y) = Complex (x * (y :+ 0.0))
  (Rational x) * (Rational y) = Rational (x * y)
  (Complex x) * (Complex y) = Complex (x * y)
  _ * _ = throw $ InvalidOperation "Cannot multiply rationals and complex numbers"

  (Integer x) - (Integer y) = Integer (x - y)
  (Integer x) - (Real y) = Real (fromInteger x - y)
  (Integer x) - (Rational y) = Rational (fromInteger x - y)
  (Integer x) - (Complex y) = Complex (fromInteger x - y)
  (Real x) - (Integer y) = Real (x - fromInteger y)
  (Rational x) - (Integer y) = Rational (x - fromInteger y)
  (Complex x) - (Integer y) = Complex (x - fromInteger y)
  (Real x) - (Real y) = Real (x - y)
  (Real x) - (Rational y) = Rational (toRational x - y)
  (Real x) - (Complex y) = Complex ((x :+ 0.0) - y)
  (Rational x) - (Real y) = Rational (x - toRational y)
  (Complex x) - (Real y) = Complex (x - (y :+ 0.0))
  (Rational x) - (Rational y) = Rational (x - y)
  (Complex x) - (Complex y) = Complex (x - y)
  _ - _ = throw $ InvalidOperation "Cannot subtract rationals and complex numbers"

  negate (Integer x) = Integer (negate x)
  negate (Real x) = Real (negate x)
  negate (Rational x) = Rational (negate x)
  negate (Complex x) = Complex (negate x)

  abs (Integer x) = Integer (abs x)
  abs (Real x) = Real (abs x)
  abs (Rational x) = Rational (abs x)
  abs (Complex x) = Complex (abs x)

  signum (Integer x) = Integer (signum x)
  signum (Real x) = Real (signum x)
  signum (Rational x) = Rational (signum x)
  signum (Complex x) = Complex (signum x)

instance Fractional Number where
  fromRational = Rational

  (Integer x) / (Integer y) = Rational (fromInteger x / fromInteger y)
  (Integer x) / (Real y) = Real (fromInteger x / y)
  (Integer x) / (Rational y) = Rational (fromInteger x / y)
  (Integer x) / (Complex y) = Complex (fromInteger x / y)
  (Real x) / (Integer y) = Real (x / fromInteger y)
  (Rational x) / (Integer y) = Rational (x / fromInteger y)
  (Complex x) / (Integer y) = Complex (x / fromInteger y)
  (Real x) / (Real y) = Real (x / y)
  (Real x) / (Rational y) = Rational (toRational x / y)
  (Real x) / (Complex y) = Complex ((x :+ 0.0) / y)
  (Rational x) / (Real y) = Rational (x / toRational y)
  (Complex x) / (Real y) = Complex (x / (y :+ 0.0))
  (Rational x) / (Rational y) = Rational (x / y)
  (Complex x) / (Complex y) = Complex (x / y)
  _ / _ = throw $ InvalidOperation "Cannot divide rationals and complex numbers"

  recip (Integer x) = Real (recip $ fromInteger x)
  recip (Real x) = Real (recip x)
  recip (Rational x) = Rational (recip x)
  recip (Complex x) = Complex (recip x)

data Fn = Fn {macro :: Bool, params :: [Text], vararg :: Maybe Text, body :: [SchemeVal], closure :: Env}

instance Show Fn where
  show _ = "<fn>"

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
  | Primitive ([SchemeVal] -> SchemeResult SchemeVal)
  | Fun Fn
  | IOFun ([SchemeVal] -> IOSchemeResult SchemeVal)
  | Port Handle
  deriving stock (Show, Typeable)

instance Eq SchemeVal where
  (==) (List x) (List y) = length x == length y && all (uncurry (==)) (zip x y)
  (==) (PairList xs x) (PairList ys y) = List (xs ++ [x]) == List (ys ++ [y])
  (==) (Vector x) (Vector y) = x == y
  (==) (Bytevector x) (Bytevector y) = x == y
  (==) (String x) (String y) = x == y
  (==) (Character x) (Character y) = x == y
  (==) (Symbol x) (Symbol y) = x == y
  (==) (Boolean x) (Boolean y) = x == y
  (==) (Number x) (Number y) = x == y
  (==) _ _ = False

unwordVals :: [SchemeVal] -> Text
unwordVals xs = T.unwords $ showVal <$> xs

showVal :: SchemeVal -> Text
showVal (List (Symbol "quote" : xs)) = "'" <> unwordVals xs
showVal (List (Symbol "quasiquote" : xs)) = "`" <> unwordVals xs
showVal (List (Symbol "unquote" : xs)) = "," <> unwordVals xs
showVal (List (Symbol "unquote-splicing" : xs)) = ",@" <> unwordVals xs
showVal (List contents) = "(" <> unwordVals contents <> ")"
showVal (PairList contents cdr) = "(" <> unwordVals contents <> " . " <> showVal cdr <> ")"
showVal (Vector vec) = T.pack $ "#(" <> unwords (map show $ elems vec) <> ")"
showVal (Bytevector vec) = T.pack $ "#u8(" <> unwords (map show $ BS.unpack vec) <> ")"
showVal (String s) = "\"" <> s <> "\""
showVal (Character a) = T.pack $ "#\\" <> [a]
showVal (Symbol s) = s
showVal (Boolean True) = "#t"
showVal (Boolean False) = "#f"
showVal (Number n) = showNum n
showVal Nil = "nil"
showVal Primitive {} = "<prim>"
showVal Fun {} = "<fun>"
showVal IOFun {} = "<io>"
showVal (Port _) = "<port>"

dumpAST :: SchemeVal -> IO ()
dumpAST = dumpAST' True

dumpAST' :: Bool -> SchemeVal -> IO ()
dumpAST' True = pPrint
dumpAST' False = pPrintLightBg

data SchemeError
  = Generic Text
  | ArgumentLengthMismatch Int [SchemeVal]
  | TypeMismatch Text SchemeVal
  | UnboundSymbol Text
  | ParserError Text
  | NotFunction SchemeVal
  | InvalidOperation Text
  | ReservedName Text
  | EmptyList
  deriving stock (Show)

instance Exception SchemeError

showError :: SchemeError -> Text
showError (TypeMismatch err vap) = "Invalid type: expected " <> err <> ", but found " <> T.pack (show vap)
showError (Generic err) = "Unexpectec error: " <> err
showError (UnboundSymbol sym) = "Unbound symbol: " <> sym
showError (ArgumentLengthMismatch ex act) = "Expected " <> T.pack (show ex) <> " but found " <> T.pack (show $ length act)
showError (ParserError err) = "Parsing error, could not parse input: " <> err
showError (NotFunction err) = "Attempt at calling " <> showVal err <> " as a function"
showError (InvalidOperation err) = "Invalid: " <> err
showError (ReservedName err) = "Cannot define : " <> err <> " because it is a reserved name"
showError EmptyList = "Empty list is not allowed"
