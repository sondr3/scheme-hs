module Scheme.Primitives.Numbers
  ( numericPrimitives,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Complex (imagPart, realPart)
import Data.Foldable (foldlM)
import Data.Text (Text)
import Scheme.Operators (unaryOperator)
import Scheme.Types (SchemeError (..), SchemeVal (..), castNum)

numericPrimitives :: [(Text, [SchemeVal] -> Either SchemeError SchemeVal)]
numericPrimitives =
  [ ("number?", unaryOperator isNumber),
    ("complex?", unaryOperator isComplex),
    ("real?", unaryOperator isReal),
    ("rational?", unaryOperator isRational),
    ("integer?", unaryOperator isInteger),
    ("exact?", unaryOperator isExact),
    ("inexact?", unaryOperator isInexact),
    ("exact-integer?", unaryOperator isExactInteger),
    ("finite?", unaryOperator isFinite),
    ("infinite?", unaryOperator isInfinite'),
    ("+", add),
    ("-", sub),
    ("*", multiply),
    ("/", division)
  ]

isNumber :: SchemeVal -> SchemeVal
isNumber (Integer _) = Boolean True
isNumber (Real _) = Boolean True
isNumber (Rational _) = Boolean True
isNumber (Complex _) = Boolean True
isNumber _ = Boolean False

isComplex :: SchemeVal -> SchemeVal
isComplex (Complex _) = Boolean True
isComplex _ = Boolean False

isReal :: SchemeVal -> SchemeVal
isReal (Real _) = Boolean True
isReal _ = Boolean False

isRational :: SchemeVal -> SchemeVal
isRational (Rational _) = Boolean True
isRational _ = Boolean False

isInteger :: SchemeVal -> SchemeVal
isInteger (Integer _) = Boolean True
isInteger _ = Boolean False

isExact :: SchemeVal -> SchemeVal
isExact (Integer _) = Boolean True
isExact (Rational _) = Boolean True
isExact _ = Boolean False

isInexact :: SchemeVal -> SchemeVal
isInexact (Real _) = Boolean True
isInexact _ = Boolean False

isExactInteger :: SchemeVal -> SchemeVal
isExactInteger x = case (isExact x, isInteger x) of
  (Boolean True, Boolean True) -> Boolean True
  _ -> Boolean False

isFinite :: SchemeVal -> SchemeVal
isFinite (Real x) = Boolean (not (isInfinite x || isNaN x))
isFinite (Complex x) = Boolean (not (isInfinite imag || isInfinite real || isNaN real || isNaN imag))
  where
    imag = imagPart x
    real = realPart x
isFinite _ = Boolean True

isInfinite' :: SchemeVal -> SchemeVal
isInfinite' (Real x) = Boolean (isInfinite x || isNaN x)
isInfinite' (Complex x) = Boolean (isInfinite imag || isInfinite real || isNaN real || isNaN imag)
  where
    imag = imagPart x
    real = realPart x
isInfinite' _ = Boolean False

add :: [SchemeVal] -> Either SchemeError SchemeVal
add [] = pure $ Integer 0
add (f : fs) = foldlM (\a b -> castNum [a, b] >>= add') f fs
  where
    add' (List [Integer x, Integer y]) = pure $ Integer (x + y)
    add' (List [Real x, Real y]) = pure $ Real (x + y)
    add' (List [Rational x, Rational y]) = pure $ Rational (x + y)
    add' (List [Complex x, Complex y]) = pure $ Complex (x + y)
    add' _ = throwError $ Generic "Something went wrong in (+)"

multiply :: [SchemeVal] -> Either SchemeError SchemeVal
multiply [] = pure $ Integer 1
multiply (f : fs) = foldlM (\a b -> castNum [a, b] >>= multiply') f fs
  where
    multiply' (List [Integer x, Integer y]) = pure $ Integer (x * y)
    multiply' (List [Real x, Real y]) = pure $ Real (x * y)
    multiply' (List [Rational x, Rational y]) = pure $ Rational (x * y)
    multiply' (List [Complex x, Complex y]) = pure $ Complex (x * y)
    multiply' _ = throwError $ Generic "Something went wrong"

sub :: [SchemeVal] -> Either SchemeError SchemeVal
sub [] = throwError $ ArgumentMismatch 1 []
sub [Integer x] = pure $ Integer (negate x)
sub [Real x] = pure $ Real (negate x)
sub [Rational x] = pure $ Rational (negate x)
sub [Complex x] = pure $ Complex (negate x)
sub (f : fs) = foldlM (\a b -> castNum [a, b] >>= sub') f fs
  where
    sub' (List [Integer x, Integer y]) = pure $ Integer (x - y)
    sub' (List [Real x, Real y]) = pure $ Real (x - y)
    sub' (List [Rational x, Rational y]) = pure $ Rational (x - y)
    sub' (List [Complex x, Complex y]) = pure $ Complex (x - y)
    sub' _ = throwError $ Generic "Something went wrong"

division :: [SchemeVal] -> Either SchemeError SchemeVal
division [] = throwError $ ArgumentMismatch 1 []
division [Integer x] = pure $ Rational (1 / fromInteger x)
division [Real x] = pure $ Real (1.0 / x)
division [Rational x] = pure $ Rational (1 / x)
division [Complex x] = pure $ Complex (1 / x)
division (f : fs) = foldlM (\a b -> castNum [a, b] >>= division') f fs
  where
    division' (List [Integer x, Integer y]) = pure $ Rational (fromInteger x / fromInteger y)
    division' (List [Real x, Real y]) = pure $ Real (x / y)
    division' (List [Rational x, Rational y]) = pure $ Rational (x / y)
    division' (List [Complex x, Complex y]) = pure $ Complex (x / y)
    division' _ = throwError $ Generic "Something went wrong"
