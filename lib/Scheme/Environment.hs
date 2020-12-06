module Scheme.Environment where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Scheme.Primitives (numericPrimitives)
import Scheme.Types (Environment, SchemeError (..), SchemeVal (..), showVal)

primitives :: [(Text, [SchemeVal] -> Either SchemeError SchemeVal)]
primitives = numericPrimitives

buildEnvironment :: Map.Map Text SchemeVal
buildEnvironment = Map.fromList (map createPrimFunc primitives)

createPrimFunc :: (Text, [SchemeVal] -> Either SchemeError SchemeVal) -> (Text, SchemeVal)
createPrimFunc (sym, func) = (sym, PrimitiveFunc func)

createFunc ::
  -- | Macro?
  Bool ->
  -- | Variadics
  Maybe Text ->
  -- | Parameters
  [SchemeVal] ->
  -- | Body
  [SchemeVal] ->
  -- | Environment closure
  Environment ->
  -- | Resulting function
  Either SchemeError SchemeVal
createFunc macro varargs params body env = pure $ Func macro (map showVal params) varargs body env

createNormalFunc ::
  -- | Parameters
  [SchemeVal] ->
  -- | Body
  [SchemeVal] ->
  Environment ->
  Either SchemeError SchemeVal
createNormalFunc = createFunc False Nothing

createVariadicFunc :: SchemeVal -> [SchemeVal] -> [SchemeVal] -> Environment -> Either SchemeError SchemeVal
createVariadicFunc = createFunc False . Just . showVal

createMacro :: [SchemeVal] -> [SchemeVal] -> Environment -> Either SchemeError SchemeVal
createMacro = createFunc True Nothing
