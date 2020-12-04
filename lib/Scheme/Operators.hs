module Scheme.Operators where

import Control.Monad.Except (MonadError, throwError)
import Scheme.Types (SchemeError (..))

unaryOperator :: MonadError SchemeError m => (t -> a) -> [t] -> m a
unaryOperator op [x] = return $ op x
unaryOperator _ _ = throwError $ Generic "Misapplied unary operator"
