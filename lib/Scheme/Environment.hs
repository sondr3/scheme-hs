module Scheme.Environment where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Scheme.Primitives (numericPrimitives)
import Scheme.Types (Env, SchemeError (..), SchemeM, SchemeVal (..), showVal)

primitives :: [(Text, [SchemeVal] -> SchemeM SchemeVal)]
primitives = numericPrimitives

buildEnvironment :: Map.Map Text SchemeVal
buildEnvironment = Map.fromList (map createPrimFunc primitives)

-- createPrimFunc :: (Text, [SchemeVal] -> Either SchemeError SchemeVal) -> (Text, SchemeVal)
-- createPrimFunc :: (a, [SchemeVal] -> Either SchemeError SchemeVal) -> (a, SchemeVal)
createPrimFunc :: (a, [SchemeVal] -> SchemeM SchemeVal) -> (a, SchemeVal)
createPrimFunc (sym, func) = (sym, Primitive func)

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
  Env ->
  -- | Resulting function
  Either SchemeError SchemeVal
createFunc macro varargs params body env = pure $ Fun macro (map showVal params) varargs body env

createNormalFunc ::
  -- | Parameters
  [SchemeVal] ->
  -- | Body
  [SchemeVal] ->
  Env ->
  Either SchemeError SchemeVal
createNormalFunc = createFunc False Nothing

createVariadicFunc :: SchemeVal -> [SchemeVal] -> [SchemeVal] -> Env -> Either SchemeError SchemeVal
createVariadicFunc = createFunc False . Just . showVal

createMacro :: [SchemeVal] -> [SchemeVal] -> Env -> Either SchemeError SchemeVal
createMacro = createFunc True Nothing
