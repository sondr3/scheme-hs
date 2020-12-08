{-# LANGUAGE ScopedTypeVariables #-}

module Scheme.Eval where

import Control.Exception (throw)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.Reader (ask, local, runReaderT)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Scheme.Environment (buildEnvironment)
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
eval (List [Symbol "lambda", List formals, body]) = undefined
-- Lambda function of the form (lambda (x y . z) z)
eval (List (Symbol "lambda" : PairList formals vararg : body)) = undefined
-- asks (Lambda (Function $ applyLambda [] body))
-- Lambda function of the form (lambda x x)
eval (List [Symbol "lambda", formal@(Symbol _), body]) = undefined
-- Application of functions :D
eval (List ((:) fun exprs)) = do
  func <- eval fun
  val <- mapM eval exprs
  case func of
    (Primitive fn) -> fn val
    -- (Fun {}) -> local (const closure) $ fn val
    _ -> throw $ NotFunction func
eval xs = throw (Generic $ "Unknown: " <> showVal xs)

evalBody :: SchemeVal -> SchemeM SchemeVal
evalBody (List [List ((:) (Symbol "define") [Symbol var, defExpr]), rest]) = do
  val <- eval defExpr
  env <- ask
  local (const $ Map.insert var val env) $ eval rest
evalBody (List ((:) (List ((:) (Symbol "define") [Symbol var, defExpr])) rest)) = do
  val <- eval defExpr
  env <- ask
  let envFn = const $ Map.insert var val env
   in local envFn $ evalBody $ List rest
evalBody body = eval body

liftThrows :: MonadError e m => Either e a -> m a
liftThrows (Right val) = return val
liftThrows (Left err) = throwError err
