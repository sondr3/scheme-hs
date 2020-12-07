module Scheme.Operators where

import Control.Exception (throw)
import Scheme.Types (Eval, Function (..), SchemeError (..), SchemeVal (..))

type Unary = SchemeVal -> Eval SchemeVal

type Binary = SchemeVal -> SchemeVal -> Eval SchemeVal

createFun :: ([SchemeVal] -> Eval SchemeVal) -> SchemeVal
createFun = Fun . Function

-- | Simple alias for unary functions
unOp :: Applicative f => (t -> a) -> [t] -> f a
unOp = unaryOperator

-- | Convert a unary operator into a monad form so 'Eval' can use it.
unaryOperator :: Applicative f => (t -> a) -> [t] -> f a
unaryOperator op [x] = pure $ op x
unaryOperator _ _ = throw $ Generic "Misapplied unary operator"
