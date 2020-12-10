module Scheme.Utils where

import Control.Monad.Except (MonadError, catchError, runExceptT, throwError)
import Data.Functor ((<&>))
import Scheme.Types (IOSchemeResult, extractValue)

liftIOThrows :: IOSchemeResult String -> IO String
liftIOThrows action = runExceptT (trapError action) <&> extractValue

liftThrows :: MonadError e m => Either e a -> m a
liftThrows (Right v) = return v
liftThrows (Left err) = throwError err

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)
