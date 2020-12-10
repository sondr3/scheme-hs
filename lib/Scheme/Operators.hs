module Scheme.Operators where

import Control.Exception (throw)
import Scheme.Types (SchemeError (..), SchemeVal (..))

type Unary = SchemeVal -> SchemeVal

type Binary = SchemeVal -> SchemeVal -> SchemeVal

-- | Simple alias for unary functions
unOp :: Applicative f => (t -> a) -> [t] -> f a
unOp = unaryOperator

binOp :: Applicative f => (t -> t -> a) -> [t] -> f a
binOp = binaryOperator

-- | Convert a unary operator into a monad form so 'Eval' can use it.
unaryOperator :: Applicative f => (t -> a) -> [t] -> f a
unaryOperator op [x] = pure $ op x
unaryOperator _ _ = throw $ Generic "Misapplied unary operator"

-- | Convert a binary operator into a monad form so 'Eval' can use it.
binaryOperator :: Applicative f => (t -> t -> a) -> [t] -> f a
binaryOperator op [x, y] = pure $ op x y
binaryOperator _ _ = throw $ Generic "Misapplied binary operator"
