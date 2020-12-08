{-# LANGUAGE ScopedTypeVariables #-}

module Scheme.Eval where

import Control.Exception (throw)
import Control.Monad (liftM, when)
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Data.Maybe (isNothing)
import Data.Text (Text)
import Scheme.Environment (bindVariables, buildEnvironment, createNormalFun, createVariadicFun, defineVariable, getVariable)
import Scheme.Parser (parseInput)
import Scheme.Types (Env, Fn (..), IOSchemeResult, SchemeError (..), SchemeVal (..), showError, showVal)

evalLine :: Text -> IO ()
evalLine input = do
  env <- buildEnvironment
  runWithEnv env (evalLineForm input) >>= print

evalLineForm :: Text -> SchemeVal
evalLineForm input = case parseInput input of
  Right val -> val
  Left err -> throw $ ParserError (showError err)

runWithEnv :: Env -> SchemeVal -> IO (Either SchemeError SchemeVal)
runWithEnv env expr = runExceptT (eval env expr)

liftThrows :: MonadError e m => Either e a -> m a
liftThrows (Right v) = return v
liftThrows (Left err) = throwError err

eval :: Env -> SchemeVal -> IOSchemeResult SchemeVal
eval _ Nil = return Nil
eval _ (List []) = return Nil
eval _ val@(Character _) = return val
eval _ val@(Number _) = return val
eval _ val@(Boolean _) = return val
eval env (Symbol sym) = getVariable sym env
eval _ (List [Symbol "quote", xs]) = return xs
eval env (List [Symbol "if", test, cons, alt]) = do
  eval env test >>= \case
    Boolean True -> eval env cons
    _ -> eval env alt
eval env (List [Symbol "if", test, cons]) = do
  eval env test >>= \case
    Boolean True -> eval env cons
    _ -> return Nil
-- Definition of the form (define〈variable〉〈expression〉
eval env (List (Symbol "define" : formal@(Symbol sym) : expr)) =
  isReserved sym >> createVariadicFun formal [] expr env >>= defineVariable env sym
-- (define (〈variable〉 〈formals〉)〈body〉[]
eval env (List (Symbol "define" : List (Symbol sym : formals) : body)) =
  isReserved sym >> createNormalFun formals body env >>= defineVariable env sym
-- (define (〈variable〉.〈formal〉)〈body〉)
eval env (List (Symbol "define" : PairList (Symbol sym : formals) vararg : body)) =
  isReserved sym >> createVariadicFun vararg formals body env >>= defineVariable env sym
-- Lambda function of the form (lambda (x y) (+ x y))
eval env (List (Symbol "lambda" : List formals : body)) = createNormalFun formals body env
-- Lambda function of the form (lambda (x y . z) z)
eval env (List (Symbol "lambda" : PairList formals vararg : body)) = createVariadicFun vararg formals body env
-- Lambda function of the form (lambda x x)
eval env (List (Symbol "lambda" : formal@(Symbol _) : body)) = createVariadicFun formal [] body env
-- Application of functions :D
eval env (List (fun : exprs)) = do
  func <- eval env fun
  case func of
    Fun Fn {macro = True} -> apply func exprs >>= eval env
    _ -> mapM (eval env) exprs >>= apply func
eval _ xs = throw (Generic $ "Unknown: " <> showVal xs)

apply :: SchemeVal -> [SchemeVal] -> IOSchemeResult SchemeVal
apply (Primitive fn) args = liftThrows $ fn args
apply (Fun (Fn _ params vararg body closure)) args
  | length params /= length args && isNothing vararg = throw $ ArgumentLengthMismatch (length params) args
  | otherwise = liftIO (bindVariables closure $ zip params args) >>= bindVararg vararg >>= evalBody
  where
    evalBody env = last <$> mapM (eval env) body
    bindVararg arg env = case arg of
      Just arg' -> liftIO $ bindVariables env [(arg', List $ drop (length params) args)]
      Nothing -> return env
apply fn _ = throwError $ NotFunction fn

isReserved :: Text -> IOSchemeResult ()
isReserved name = when (name `elem` ["define", "lambda", "if"]) $ throw $ ReservedName name
