module Scheme.Eval where

import Control.Monad.Reader (Reader, ask, asks, runReader)
import qualified Data.Map.Strict as Map
import Scheme.Types (Environment, SchemeVal (..))

evaluate :: SchemeVal -> SchemeVal
evaluate vals = runReader (eval vals) Map.empty

eval :: SchemeVal -> Reader Environment SchemeVal
eval val@(String _) = return val
eval val@(Character _) = return val
eval val@(Integer _) = return val
eval val@(Real _) = return val
eval val@(Rational _) = return val
eval val@(Complex _) = return val
eval val@(Boolean _) = return val
eval (Symbol s) = do
  sym <- asks (Map.lookup s)
  case sym of
    Just val -> return val
    Nothing -> throwError $ UnboundSymbol s
eval (List [Symbol "quote", xs]) = return xs
eval (List [Symbol "if", test, cons, alt]) = do
  eval test >>= \case
    Boolean True -> eval cons
    _ -> eval alt
eval (List [Symbol "if", test, cons]) = do
  eval test >>= \case
    Boolean True -> eval cons
    _ -> return Nil
eval (List (Symbol "lambda" : List formals : body)) = do
  env <- ask
  return $ Procedure env (List formals) body
eval (List (Symbol "lambda" : Symbol formal : body)) = do
  env <- ask
  return $ Procedure env (Symbol formal) body
eval (List (fun : args)) = do
  f <- eval fun
  mapM eval args >>= apply f
eval _ = undefined

apply :: MonadError SchemeError m => SchemeVal -> [SchemeVal] -> m SchemeVal
apply (PrimitiveExpression fun) args = liftThrows $ fun args
apply _ _ = undefined

liftThrows :: MonadError e m => Either e a -> m a
liftThrows (Right val) = return val
liftThrows (Left err) = throwError err
