module Scheme.Primitives.Numbers
  ( numericPrimitives,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Foldable (foldlM)
import Data.Text (Text)
import Scheme.Types (SchemeError (..), SchemeVal (..), castNum)

numericPrimitives :: [(Text, [SchemeVal] -> Either SchemeError SchemeVal)]
numericPrimitives =
  [ ("+", add),
    ("-", sub),
    ("*", multiply),
    ("/", division)
  ]

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
