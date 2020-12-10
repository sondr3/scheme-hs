module Scheme.Utils where

import Control.Monad.Except (MonadError, catchError, liftIO, runExceptT, throwError)
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T
import Scheme.Parser (readManyExpr)
import Scheme.Types (IOSchemeResult, SchemeVal, extractValue)

liftIOThrows :: IOSchemeResult String -> IO String
liftIOThrows action = runExceptT (trapError action) <&> extractValue

liftThrows :: MonadError e m => Either e a -> m a
liftThrows (Right v) = return v
liftThrows (Left err) = throwError err

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

load :: Text -> IOSchemeResult [SchemeVal]
load filename = liftIO (T.pack <$> readFile (T.unpack filename)) >>= liftThrows . readManyExpr
