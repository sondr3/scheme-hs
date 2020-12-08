module Scheme.Environment where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Scheme.Primitives (numericPrimitives)
import Scheme.Types (Env, SchemeM, SchemeVal (..), showVal)

primitives :: [(Text, [SchemeVal] -> SchemeM SchemeVal)]
primitives = numericPrimitives

buildEnvironment :: Map.Map Text SchemeVal
buildEnvironment = Map.fromList (map createPrimFun primitives)

-- createPrimFun :: (Text, [SchemeVal] -> Either SchemeError SchemeVal) -> (Text, SchemeVal)
-- createPrimFun :: (a, [SchemeVal] -> Either SchemeError SchemeVal) -> (a, SchemeVal)
createPrimFun :: (a, [SchemeVal] -> SchemeM SchemeVal) -> (a, SchemeVal)
createPrimFun (sym, func) = (sym, Primitive func)

createFun ::
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
  SchemeVal
createFun macro varargs params = Fun macro (map showVal params) varargs

createNormalFun ::
  -- | Parameters
  [SchemeVal] ->
  -- | Body
  [SchemeVal] ->
  Env ->
  SchemeVal
createNormalFun = createFun False Nothing

createVariadicFun :: SchemeVal -> [SchemeVal] -> [SchemeVal] -> Env -> SchemeVal
createVariadicFun = createFun False . Just . showVal

createMacro :: [SchemeVal] -> [SchemeVal] -> Env -> SchemeVal
createMacro = createFun True Nothing

bindVariables :: Env -> [(Text, SchemeVal)] -> Env
bindVariables old closure = Map.union old (Map.fromList closure)
