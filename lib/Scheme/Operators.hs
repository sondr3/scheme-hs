module Scheme.Operators where

import Control.Exception (throw)
import Scheme.Types (SchemeError (..), SchemeVal (..))

type Unary = SchemeVal -> SchemeVal

type Binary = SchemeVal -> SchemeVal -> SchemeVal

-- | Simple alias for unary functions
unOp :: Applicative f => (t -> a) -> [t] -> f a
unOp = unaryOperator

-- | Convert a unary operator into a monad form so 'Eval' can use it.
unaryOperator :: Applicative f => (t -> a) -> [t] -> f a
unaryOperator op [x] = pure $ op x
unaryOperator _ _ = throw $ Generic "Misapplied unary operator"
