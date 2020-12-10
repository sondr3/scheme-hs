module Scheme.Primitives.Numbers
  ( numericPrimitives,
  )
where

import Control.Exception (throw)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Complex (imagPart, realPart)
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import Scheme.Operators (binOp, unOp, unaryOperator)
import Scheme.Types (Number (..), SchemeError (..), SchemeResult, SchemeVal (..))

numericPrimitives :: [(Text, [SchemeVal] -> SchemeResult SchemeVal)]
numericPrimitives =
  [ ("number?", unOp isNumber),
    ("complex?", unOp isComplex),
    ("real?", unOp isReal),
    ("rational?", unOp isRational),
    ("integer?", unOp isInteger),
    ("exact?", unOp isExact),
    ("inexact?", unOp isInexact),
    ("exact-integer?", unOp isExactInteger),
    ("finite?", unOp isFinite),
    ("infinite?", unOp isInfinite'),
    ("nan?", unOp isNaN'),
    ("=", numericBoolOp (==)),
    ("<", numericBoolOp (<)),
    (">", numericBoolOp (>)),
    ("<=", numericBoolOp (<=)),
    (">=", numericBoolOp (>=)),
    ("+", add),
    ("-", sub),
    ("*", multiply),
    ("/", division),
    ("abs", unOp numAbs),
    ("floor/", floorInt)
  ]

isNumber :: SchemeVal -> SchemeVal
isNumber (Number (Integer _)) = Boolean True
isNumber (Number (Real _)) = Boolean True
isNumber (Number (Rational _)) = Boolean True
isNumber (Number (Complex _)) = Boolean True
isNumber _ = Boolean False

isComplex :: SchemeVal -> SchemeVal
isComplex (Number (Integer _)) = Boolean True
isComplex (Number (Real _)) = Boolean True
isComplex (Number (Rational _)) = Boolean True
isComplex (Number (Complex _)) = Boolean True
isComplex _ = Boolean False

isReal :: SchemeVal -> SchemeVal
isReal (Number (Real _)) = Boolean True
isReal (Number (Rational _)) = Boolean True
isReal (Number (Integer _)) = Boolean True
isReal (Number (Complex x)) = if imagPart x == 0 then Boolean True else Boolean False
isReal _ = Boolean False

isRational :: SchemeVal -> SchemeVal
isRational (Number (Rational _)) = Boolean True
isRational (Number (Integer _)) = Boolean True
isRational (Number (Real x)) = Boolean (not $ isInfinite x)
isRational _ = Boolean False

isInteger :: SchemeVal -> SchemeVal
isInteger (Number (Integer _)) = Boolean True
isInteger (Number (Real x)) = Boolean (isDoubleInt x)
isInteger (Number (Rational x)) = Boolean (numerator x >= denominator x && numerator x `mod` denominator x == 0)
isInteger (Number (Complex x)) = Boolean (isDoubleInt (realPart x) && isDoubleInt (imagPart x))
isInteger _ = Boolean False

isExact :: SchemeVal -> SchemeVal
isExact (Number (Integer _)) = Boolean True
isExact (Number (Rational _)) = Boolean True
isExact _ = Boolean False

isInexact :: SchemeVal -> SchemeVal
isInexact (Number (Real _)) = Boolean True
isInexact _ = Boolean False

isExactInteger :: SchemeVal -> SchemeVal
isExactInteger x = case (isExact x, isInteger x) of
  (Boolean True, Boolean True) -> Boolean True
  _ -> Boolean False

isFinite :: SchemeVal -> SchemeVal
isFinite (Number (Real x)) = Boolean (not (isInfinite x || isNaN x))
isFinite (Number (Complex x)) = Boolean (not (isInfinite imag || isInfinite real || isNaN real || isNaN imag))
  where
    imag = imagPart x
    real = realPart x
isFinite _ = Boolean True

isInfinite' :: SchemeVal -> SchemeVal
isInfinite' (Number (Real x)) = Boolean (isInfinite x || isNaN x)
isInfinite' (Number (Complex x)) = Boolean (isInfinite imag || isInfinite real || isNaN real || isNaN imag)
  where
    imag = imagPart x
    real = realPart x
isInfinite' _ = Boolean False

isNaN' :: SchemeVal -> SchemeVal
isNaN' (Number (Real x)) = Boolean (isNaN x)
isNaN' (Number (Complex x)) = Boolean (isNaN (imagPart x) || isNaN (realPart x))
isNaN' _ = Boolean False

add :: [SchemeVal] -> SchemeResult SchemeVal
add [] = pure $ Number $ Integer 0
add xs = do
  nums <- mapM unwrapNumber xs
  return $ Number (sum nums)

multiply :: [SchemeVal] -> SchemeResult SchemeVal
multiply [] = pure $ Number $ Integer 1
multiply xs = do
  nums <- mapM unwrapNumber xs
  return $ Number (product nums)

sub :: [SchemeVal] -> SchemeResult SchemeVal
sub [] = throw $ ArgumentLengthMismatch 1 []
sub [x] = do
  num <- unwrapNumber x
  return $ Number (negate num)
sub xs = do
  nums <- mapM unwrapNumber xs
  return $ Number (foldl1 (-) nums)

division :: [SchemeVal] -> SchemeResult SchemeVal
division [] = throw $ ArgumentLengthMismatch 1 []
division [x] = do
  num <- unwrapNumber x
  return $ Number (recip num)
division xs = do
  nums <- mapM unwrapNumber xs
  return $ Number (foldl1 (/) nums)

numAbs :: SchemeVal -> SchemeVal
numAbs (Number n) = Number $ abs n
numAbs x = throw $ TypeMismatch "number" x

floorInt :: [SchemeVal] -> SchemeResult SchemeVal
floorInt [Number (Integer x), Number (Integer y)] = return $ List [Number $ Integer x', Number $ Integer y']
  where
    (x', y') = x `divMod` y
floorInt [x, Number (Integer _)] = throw $ TypeMismatch "integer" x
floorInt [Number (Integer _), x] = throw $ TypeMismatch "integer" x
floorInt _ = throw $ ArgumentLengthMismatch 2 []

numericBoolOp :: (Number -> Number -> Bool) -> [SchemeVal] -> Either SchemeError SchemeVal
numericBoolOp _ [] = throw $ ArgumentLengthMismatch 1 []
numericBoolOp _ [x] = case isNaN' x of
  Boolean True -> return $ Boolean False
  _ -> return $ Boolean True
numericBoolOp op xs = do
  nums <- mapM unwrapNumber xs
  return $ Boolean $ and $ zipWith op nums $ drop 1 nums

isDoubleInt :: Double -> Bool
isDoubleInt d = (ceiling d :: Integer) == (floor d :: Integer)

unwrapNumber :: SchemeVal -> SchemeResult Number
unwrapNumber (Number x) = pure x
unwrapNumber x = throw $ TypeMismatch "number" x
