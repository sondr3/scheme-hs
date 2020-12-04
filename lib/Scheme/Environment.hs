module Scheme.Environment where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Scheme.Primitives (numericPrimitives)
import Scheme.Types (SchemeError (..), SchemeVal (..))

primitives :: [(Text, [SchemeVal] -> Either SchemeError SchemeVal)]
primitives = numericPrimitives

buildEnvironment :: Map.Map Text SchemeVal
buildEnvironment = Map.fromList (map buildFunc primitives)

buildFunc :: (Text, [SchemeVal] -> Either SchemeError SchemeVal) -> (Text, SchemeVal)
buildFunc (sym, func) = (sym, PrimitiveExpression func)
