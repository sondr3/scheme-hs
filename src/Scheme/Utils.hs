module Scheme.Utils
  ( liftIOThrows,
    liftThrows,
    trapError,
    load,
    -- new
    load',
  )
where

import Control.Exception (throw)
import Control.Monad.Except (MonadError, catchError, liftIO, runExceptT, throwError)
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Scheme.Parser (readManyExpr)
import Scheme.Types (IOSchemeResult, SchemeResult, SchemeVal, extractValue)

liftIOThrows :: IOSchemeResult String -> IO String
liftIOThrows action = runExceptT (trapError action) <&> extractValue

liftThrows :: SchemeResult a -> IOSchemeResult a
liftThrows (Right v) = return v
liftThrows (Left err) = throwError err

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

load' :: Text -> IO [SchemeVal]
load' filename = do
  content <- TIO.readFile $ T.unpack filename
  case readManyExpr content of
    Right vals -> pure vals
    Left err -> throw err

-- readFile (T.unpack filename) >>= readManyExpr . T.pack

load :: Text -> IOSchemeResult [SchemeVal]
load filename = liftIO (T.pack <$> readFile (T.unpack filename)) >>= liftThrows . readManyExpr
