module Scheme.Types where

import Control.Monad.Except (MonadError (throwError))
import Data.Array (Array, elems)
import qualified Data.ByteString as BS
import Data.Complex (Complex, imagPart, realPart)
import qualified Data.Map.Strict as Map
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pretty.Simple (pPrint, pPrintLightBg)
import Text.Show.Functions ()

type Environment = Map.Map Text SchemeVal

data SchemeVal
  = List [SchemeVal]
  | PairList [SchemeVal] SchemeVal
  | Vector (Array Int SchemeVal)
  | Bytevector BS.ByteString
  | String Text
  | Character Char
  | Symbol Text
  | Boolean Bool
  | Integer Integer
  | Real Double
  | Rational Rational
  | Complex (Complex Double)
  | Nil
  | -- | Definition for primitive functions
    PrimitiveFunc ([SchemeVal] -> Either SchemeError SchemeVal)
  | -- | Definition for all other functions
    Func
      { macro :: Bool,
        params :: [Text],
        vararg :: Maybe Text,
        body :: [SchemeVal],
        closure :: Environment
      }
  deriving (Show)

instance Eq SchemeVal where
  (==) (List x) (List y) = length x == length y && all (uncurry (==)) (zip x y)
  (==) (PairList xs x) (PairList ys y) = List (xs ++ [x]) == List (ys ++ [y])
  (==) (Vector x) (Vector y) = x == y
  (==) (Bytevector x) (Bytevector y) = x == y
  (==) (String x) (String y) = x == y
  (==) (Character x) (Character y) = x == y
  (==) (Symbol x) (Symbol y) = x == y
  (==) (Boolean x) (Boolean y) = x == y
  (==) (Integer x) (Integer y) = x == y
  (==) (Real x) (Real y) = x == y
  (==) (Rational x) (Rational y) = x == y
  (==) (Complex x) (Complex y) = x == y
  (==) _ _ = False

castNum :: [SchemeVal] -> Either SchemeError SchemeVal
castNum [x@(Integer _), y@(Integer _)] = return $ List [x, y]
castNum [x@(Real _), y@(Real _)] = return $ List [x, y]
castNum [x@(Rational _), y@(Rational _)] = return $ List [x, y]
castNum [x@(Complex _), y@(Complex _)] = return $ List [x, y]
castNum [Integer x, y@(Real _)] = return $ List [Real $ fromInteger x, y]
castNum [x@(Real _), Integer y] = return $ List [x, Real $ fromInteger y]
castNum [Integer x, y@(Rational _)] = return $ List [Rational $ fromInteger x, y]
castNum [x@(Rational _), Integer y] = return $ List [x, Rational $ fromInteger y]
castNum [Integer x, y@(Complex _)] = return $ List [Complex $ fromInteger x, y]
castNum [x@(Complex _), Integer y] = return $ List [x, Complex $ fromInteger y]
castNum [x@(Rational _), Real y] = return $ List [x, Rational $ toRational y]
castNum [Real x, y@(Rational _)] = return $ List [Rational $ toRational x, y]
castNum x = throwError $ TypeMismatch "what" (head x)

showVal :: SchemeVal -> Text
showVal (List (Symbol "quote" : xs)) = "'" <> T.unwords (map showVal xs)
showVal (List (Symbol "quasiquote" : xs)) = "`" <> T.unwords (map showVal xs)
showVal (List (Symbol "unquote" : xs)) = "," <> T.unwords (map showVal xs)
showVal (List (Symbol "unquote-splicing" : xs)) = ",@" <> T.unwords (map showVal xs)
showVal (List contents) = "(" <> T.unwords (map showVal contents) <> ")"
showVal (PairList contents cdr) = "(" <> T.unwords (map showVal contents) <> " . " <> T.pack (show cdr) <> ")"
showVal (Vector vec) = T.pack $ "#(" <> unwords (map show $ elems vec) <> ")"
showVal (Bytevector vec) = T.pack $ "#u8(" <> unwords (map show $ BS.unpack vec) <> ")"
showVal (String s) = s
showVal (Character a) = T.pack $ "#\\" <> [a]
showVal (Symbol s) = s
showVal (Boolean True) = "#t"
showVal (Boolean False) = "#f"
showVal (Integer i) = T.pack $show i
showVal (Real d) = T.pack $show d
showVal (Rational r) = T.pack $show (numerator r) <> "/" <> show (denominator r)
showVal (Complex p) = T.pack $ show (realPart p) <> "+" <> show (imagPart p) <> "i"
showVal Nil = "nil"
showVal PrimitiveFunc {} = "<prim>"
showVal Func {} = "<func>"

dumpAST :: SchemeVal -> IO ()
dumpAST = dumpAST' True

dumpAST' :: Bool -> SchemeVal -> IO ()
dumpAST' True = pPrint
dumpAST' False = pPrintLightBg

data SchemeError
  = Generic Text
  | ArgumentMismatch Int [SchemeVal]
  | TypeMismatch Text SchemeVal
  | UnboundSymbol Text
  deriving (Show)

showError :: SchemeError -> Text
showError (TypeMismatch err vap) = "Invalid type: expected " <> err <> ", but found " <> T.pack (show vap)
showError (Generic err) = "Unexpectec error: " <> err
showError (UnboundSymbol sym) = "Unbound symbol: " <> sym
showError (ArgumentMismatch ex act) = "Expected " <> T.pack (show ex) <> " but found " <> T.pack (show $ length act)
