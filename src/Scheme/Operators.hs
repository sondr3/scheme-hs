module Scheme.Operators (unOp, binOp, Unary, Binary) where

import Control.Monad.Except (MonadError (throwError))
import Scheme.Types (SchemeError (..), SchemeVal (..))

type Unary = SchemeVal -> SchemeVal

type Binary = SchemeVal -> SchemeVal -> SchemeVal

-- | Simple alias for unary functions
unOp :: (MonadError SchemeError f) => (t -> a) -> [t] -> f a
unOp = unaryOperator

binOp :: (MonadError SchemeError f) => (t -> t -> a) -> [t] -> f a
binOp = binaryOperator

-- | Convert a unary operator into a monad form so 'Eval' can use it.
unaryOperator :: (MonadError SchemeError f) => (t -> a) -> [t] -> f a
unaryOperator op [x] = pure $ op x
unaryOperator _ _ = throwError $ Generic "Misapplied unary operator"

-- | Convert a binary operator into a monad form so 'Eval' can use it.
binaryOperator :: (MonadError SchemeError f) => (t -> t -> a) -> [t] -> f a
binaryOperator op [x, y] = pure $ op x y
binaryOperator _ _ = throwError $ Generic "Misapplied binary operator"
