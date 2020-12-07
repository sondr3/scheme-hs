module Scheme.Environment where

import Control.Exception (throw)
import Control.Monad.Reader (MonadReader (ask))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Scheme.Primitives (numericPrimitives)
import Scheme.Types (Eval, SchemeError (..), SchemeVal (..))

primitives :: [(Text, SchemeVal)]
primitives = numericPrimitives

buildEnvironment :: Map.Map Text SchemeVal
buildEnvironment = Map.fromList primitives

getVariable :: SchemeVal -> Eval SchemeVal
getVariable (Symbol name) = do
  env <- ask
  case Map.lookup name env of
    Just val -> return val
    Nothing -> throw $ UnboundSymbol name
getVariable _ = throw $ Generic "Attempt to lookup variable with invalid type"

extractVariable :: SchemeVal -> Text
extractVariable (Symbol sym) = sym
extractVariable _ = throw $ Generic "Expected symbol"
