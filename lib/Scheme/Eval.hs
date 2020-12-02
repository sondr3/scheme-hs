module Scheme.Eval where

import Control.Monad.Reader (Reader, ask, asks, runReader)
import qualified Data.Map.Strict as Map
import Scheme.Types (Environment, SchemeVal (..))

evaluate :: SchemeVal -> SchemeVal
evaluate vals = runReader (eval vals) Map.empty

eval :: SchemeVal -> Reader Environment SchemeVal
eval val@(String _) = return val
eval val@(Character _) = return val
eval val@(Number _) = return val
eval val@(Boolean _) = return val
eval (Symbol s) = do
  e <- asks (Map.lookup s)
  case e of
    Just val -> return val
    Nothing -> error "Unbound variable"
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
eval _ = undefined
