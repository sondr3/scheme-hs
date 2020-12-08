module Scheme.Environment where

import Control.Exception (throw)
import Control.Monad.Except (MonadIO (liftIO), throwError)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Scheme.Primitives (numericPrimitives)
import Scheme.Types (Env, Fn (..), IOSchemeResult, SchemeError (..), SchemeResult, SchemeVal (..), showVal)

primitives :: [(Text, [SchemeVal] -> SchemeResult SchemeVal)]
primitives = numericPrimitives

nullEnv :: IO Env
nullEnv = newIORef []

buildEnvironment :: IO Env
buildEnvironment = nullEnv >>= flip bindVariables (map createPrimFun primitives)

createPrimFun :: (a, [SchemeVal] -> SchemeResult SchemeVal) -> (a, SchemeVal)
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
  IOSchemeResult SchemeVal
createFun macro varargs params body closure = return $ Fun $ Fn macro (map showVal params) varargs body closure

createNormalFun ::
  -- | Parameters
  [SchemeVal] ->
  -- | Body
  [SchemeVal] ->
  Env ->
  IOSchemeResult SchemeVal
createNormalFun = createFun False Nothing

createVariadicFun :: SchemeVal -> [SchemeVal] -> [SchemeVal] -> Env -> IOSchemeResult SchemeVal
createVariadicFun = createFun False . Just . showVal

createMacro :: [SchemeVal] -> [SchemeVal] -> Env -> IOSchemeResult SchemeVal
createMacro = createFun True Nothing

isBound :: Env -> Text -> IO Bool
isBound envRef var = do
  env <- readIORef envRef
  case lookup var env of
    Just _ -> return True
    Nothing -> return False

withVar :: Env -> Text -> IOSchemeResult SchemeVal
withVar envRef var = do
  env <- liftIO $ readIORef envRef
  case lookup var env of
    Just val -> liftIO $ readIORef val
    Nothing -> throw $ UnboundSymbol var

getVariable :: Text -> Env -> IOSchemeResult SchemeVal
getVariable var envRef = withVar envRef var

setVariable :: Env -> Text -> SchemeVal -> IOSchemeResult SchemeVal
setVariable envRef var val = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundSymbol var) (liftIO . flip writeIORef val) (lookup var env)
  return val

defineVariable :: Env -> Text -> SchemeVal -> IOSchemeResult SchemeVal
defineVariable envRef var val = do
  isDefined <- liftIO $ isBound envRef var
  if isDefined
    then setVariable envRef var val >> return val
    else liftIO $ do
      valRef <- newIORef val
      env <- readIORef envRef
      writeIORef envRef ((var, valRef) : env)
      return val

bindVariables :: Env -> [(Text, SchemeVal)] -> IO Env
bindVariables envRef closure = readIORef envRef >>= extendEnv closure >>= newIORef
  where
    extendEnv cls env = fmap (++ env) (mapM addBinding cls)
    addBinding (var, val) = do
      valRef <- newIORef val
      return (var, valRef)
