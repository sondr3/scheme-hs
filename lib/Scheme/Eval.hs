{-# LANGUAGE ScopedTypeVariables #-}

module Scheme.Eval where

import Control.Exception (throw)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ask, asks, local, runReaderT)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import Scheme.Environment (bindVariables, buildEnvironment, createNormalFun, createVariadicFun)
import Scheme.Parser (parseInput)
import Scheme.Types (Env, SchemeError (..), SchemeM, SchemeVal (..), showError, showVal, unScheme)

evalLine :: Text -> IO ()
evalLine input = runWithEnv buildEnvironment (lineToSchemeM input) >>= print

lineToSchemeM :: Text -> SchemeM SchemeVal
lineToSchemeM input = either (throw . ParserError . showError) eval $ parseInput input

runWithEnv :: Env -> SchemeM a -> IO (Either SchemeError a)
runWithEnv env expr = runExceptT (runReaderT (unScheme expr) env)

eval :: SchemeVal -> SchemeM SchemeVal
eval Nil = return Nil
eval (List []) = return Nil
eval val@(Character _) = return val
eval val@(Number _) = return val
eval val@(Boolean _) = return val
eval (Symbol sym) = do
  env <- ask
  case Map.lookup sym env of
    Just x -> return x
    Nothing -> throw $ UnboundSymbol sym
eval (List [Symbol "quote", xs]) = return xs
eval (List [Symbol "if", test, cons, alt]) = do
  eval test >>= \case
    Boolean True -> eval cons
    _ -> eval alt
eval (List [Symbol "if", test, cons]) = do
  eval test >>= \case
    Boolean True -> eval cons
    _ -> return Nil
-- Definition of the form (define〈variable〉〈expression〉
eval (List [Symbol "define", var@(Symbol name), expr]) = do
  env <- ask
  val <- eval expr
  local (const (Map.insert name val env)) (pure var)
-- (define (〈variable〉 〈formals〉)〈body〉[]
eval (List (Symbol "define" : List (Symbol variable : formals) : body)) = undefined
-- (define (〈variable〉.〈formal〉)〈body〉)
eval (List (Symbol "define" : PairList (Symbol variable : formals) vararg : body)) = undefined
-- Lambda function of the form (lambda (x y) (+ x y))
eval (List (Symbol "lambda" : List formals : body)) = asks (createNormalFun formals body)
-- Lambda function of the form (lambda (x y . z) z)
eval (List (Symbol "lambda" : PairList formals vararg : body)) = asks (createVariadicFun vararg formals body)
-- asks (Lambda (Function $ applyLambda [] body))
-- Lambda function of the form (lambda x x)
eval (List (Symbol "lambda" : formal@(Symbol _) : body)) = asks (createVariadicFun formal [] body)
-- Application of functions :D
eval (List (fun : exprs)) = eval fun >>= \fn -> evalMany exprs >>= apply fn
eval xs = throw (Generic $ "Unknown: " <> showVal xs)

evalMany :: [SchemeVal] -> SchemeM [SchemeVal]
evalMany = traverse eval

evalBody' :: [SchemeVal] -> SchemeM SchemeVal
evalBody' body = last <$> evalMany body

apply :: SchemeVal -> [SchemeVal] -> SchemeM SchemeVal
apply (Primitive fn) args = fn args
apply (Fun _ params vararg body closure) args
  | length params /= length args && isNothing vararg = throw $ ArgumentLengthMismatch (length params) args
  | otherwise = do
    let vars = bindVariables closure $ zip params args
        env = bindVararg vararg vars
     in local (const env) $ evalBody' body
  where
    bindVararg arg env = case arg of
      Just arg' -> bindVariables env [(arg', List $ drop (length params) args)]
      Nothing -> env
apply fn _ = throw $ NotFunction fn

isReserved :: Text -> IOSchemeResult ()
isReserved name = when (name `elem` ["define", "lambda", "if"]) $ throw $ ReservedName name
