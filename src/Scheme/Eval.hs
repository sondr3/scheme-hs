{-# LANGUAGE ScopedTypeVariables #-}

module Scheme.Eval
  ( eval,
    runWithEnv,
    primitiveNames,
    primitives,
    buildEnvironment,
    evalLine,
    evalLineForm,
    loadStdLib,
  )
where

import Control.Exception (throw)
import Control.Monad (void, when)
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Except (runExceptT, throwError)
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Scheme.Environment (bindVariables, createMacro, createNormalFun, createVariadicFun, defineVariable, getVariable, getVariables, nullEnv)
import Scheme.Parser (parseInput)
import Scheme.Primitives (booleanPrimitives, equivalencePrimitives, ioPrimitives, listPrimitives, numericPrimitives, stringPrimitives, symbolPrimitives)
import Scheme.Primitives.Eval (evalPrimitives)
import Scheme.Types (Env, Fn (..), IOSchemeResult, SchemeError (..), SchemeResult, SchemeVal (..), showError, showVal)
import Scheme.Utils (liftIOThrows, liftThrows, load)

primitiveNames :: [String]
primitiveNames =
  map (T.unpack . fst) primitives
    ++ map T.unpack evalPrimitives
    ++ map (T.unpack . fst) ioPrimitives

primitives :: [(Text, [SchemeVal] -> SchemeResult SchemeVal)]
primitives =
  numericPrimitives
    ++ stringPrimitives
    ++ listPrimitives
    ++ symbolPrimitives
    ++ equivalencePrimitives
    ++ booleanPrimitives

buildEnvironment :: IO Env
buildEnvironment =
  nullEnv
    >>= flip
      bindVariables
      ( map
          createIOFun
          ( ioPrimitives
              ++ [("apply", applyProc)]
          )
          ++ map createPrimFun primitives
      )

createPrimFun :: (a, [SchemeVal] -> SchemeResult SchemeVal) -> (a, SchemeVal)
createPrimFun (sym, func) = (sym, Primitive func)

createIOFun :: (a, [SchemeVal] -> IOSchemeResult SchemeVal) -> (a, SchemeVal)
createIOFun (sym, func) = (sym, IOFun func)

evalLine :: Text -> IO ()
evalLine input = do
  env <- buildEnvironment
  void (loadStdLib env)
  runWithEnv env (evalLineForm input) >>= print

evalLineForm :: Text -> SchemeVal
evalLineForm input = case parseInput input of
  Right val -> val
  Left err -> throw $ ParserError (showError err)

runWithEnv :: Env -> SchemeVal -> IO (Either SchemeError SchemeVal)
runWithEnv env expr = runExceptT (eval env expr)

loadStdLib :: Env -> IO String
loadStdLib env = liftIOThrows (show <$> eval env (List [Symbol "load", String "stdlib.scm"]))

eval :: Env -> SchemeVal -> IOSchemeResult SchemeVal
eval _ Nil = return Nil
eval _ (List []) = return Nil
eval _ val@(Character _) = return val
eval _ val@(Number _) = return val
eval _ val@(Boolean _) = return val
eval _ val@(String _) = return val
eval env (Symbol sym) = getVariable env sym
eval env (List [Symbol "load", String filename]) = load filename >>= fmap last . mapM (eval env)
eval env (List [Symbol "interaction-environment"]) = do
  bindings <- getVariables env
  return $ List (map toPair bindings)
  where
    toPair (var, val) = List [Symbol var, val]
eval _ (List [Symbol "quote", xs]) = return xs
eval env (List [Symbol "quasiquote", xs]) = do
  evalUnquotes xs
  where
    evalUnquotes (List [Symbol "unquote", expr]) = eval env expr
    evalUnquotes (List items) = List <$> traverse evalUnquotes items
    evalUnquotes form = return form
eval env (List [Symbol "if", test, cons, alt]) = do
  eval env test >>= \case
    Boolean True -> eval env cons
    _ -> eval env alt
eval env (List [Symbol "if", test, cons]) = do
  eval env test >>= \case
    Boolean True -> eval env cons
    _ -> return Nil
eval env (List (Symbol "cond" : forms))
  | null forms = throwError $ InvalidOperation "No true clause in cond"
  | otherwise = case head forms of
      List (Symbol "else" : exprs) -> if null exprs then return $ List [] else mapM (eval env) exprs >>= liftThrows . return . last
      List [test, cons] -> eval env $ List [Symbol "if", test, cons, List (Symbol "cond" : tail forms)]
      _ -> throwError $ Generic "Could not evaluate cond expression"
-- (set!〈variable〉〈expression〉)
eval env (List [Symbol "set!", Symbol sym, expr]) =
  isReserved sym >> eval env expr >>= defineVariable env sym >> return Nil
-- Definition of the form (define〈variable〉〈expression〉
eval env (List (Symbol "define-syntax" : List (Symbol var : params) : body)) =
  createMacro params body env >>= defineVariable env var
eval env (List [Symbol "define", Symbol sym, expr]) =
  isReserved sym >> eval env expr >>= defineVariable env sym
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
eval _ xs = throwError (Generic $ "Unknown: " <> showVal xs)

applyProc :: [SchemeVal] -> IOSchemeResult SchemeVal
applyProc [fun, List args] = apply fun args
applyProc [fun, PairList args rest] = applyProc [fun, List (args ++ [rest])]
applyProc (fun : args) = apply fun args
applyProc _ = throwError $ InvalidOperation "apply-proc"

apply :: SchemeVal -> [SchemeVal] -> IOSchemeResult SchemeVal
apply (Primitive fn) args = liftThrows $ fn args
apply (IOFun fn) args = fn args
apply (Fun (Fn _ params vararg body closure)) args
  | length params /= length args && isNothing vararg = throwError $ ArgumentLengthMismatch (length params) args
  | otherwise = liftIO (bindVariables closure $ zip params args) >>= bindVararg vararg >>= evalBody
  where
    evalBody env = last <$> mapM (eval env) body
    bindVararg arg env = case arg of
      Just arg' -> liftIO $ bindVariables env [(arg', List $ drop (length params) args)]
      Nothing -> return env
apply fn _ = throwError $ NotFunction fn

isReserved :: Text -> IOSchemeResult ()
isReserved name = when (name `elem` ["define", "lambda", "if"]) $ throwError $ ReservedName name
