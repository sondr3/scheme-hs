{-# LANGUAGE ScopedTypeVariables #-}

module Scheme.Eval where

import Control.Exception (SomeException, fromException, throw, try)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (ask, asks, local, runReaderT)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Scheme.Environment (buildEnvironment, extractVariable, getVariable)
import Scheme.Parser (parseFile, parseInput)
import Scheme.Types (Environment, Eval, Function (..), SchemeError (..), SchemeVal (..), showError, showVal, unEval)

safeExex :: IO a -> IO (Either String a)
safeExex m = do
  res <- try m
  case res of
    Left (err :: SomeException) ->
      case fromException err of
        Just (enclosed :: SchemeError) -> return $ Left (show enclosed)
        Nothing -> return $ Left (show err)
    Right val -> return $ Right val

evalFile :: FilePath -> Text -> IO ()
evalFile file content = runInEnv buildEnvironment (fileToEvalForm file content) >>= print

evalLine :: Text -> IO ()
evalLine content = runInEnv buildEnvironment (lineToEvalForm content) >>= print

lineToEvalForm :: Text -> Eval SchemeVal
lineToEvalForm content = either (throw . ParserError . showError) eval $ parseInput content

fileToEvalForm :: FilePath -> Text -> Eval SchemeVal
fileToEvalForm file content = either (throw . ParserError . showError) eval $ parseFile file content

showParse :: Text -> Text
showParse input = either showError showVal $ parseInput input

runInEnv :: Environment -> Eval b -> IO b
runInEnv code action = runReaderT (unEval action) code

ensureSymbol :: SchemeVal -> Eval SchemeVal
ensureSymbol n@(Symbol _) = pure n
ensureSymbol n = throw $ TypeMismatch "symbol" n

bindArgsEval :: [SchemeVal] -> [SchemeVal] -> SchemeVal -> Eval SchemeVal
bindArgsEval params args expr = do
  env <- ask
  local (const (Map.fromList (zipWith (\a b -> (extractVariable a, b)) params args) <> env)) $ eval expr

eval :: SchemeVal -> Eval SchemeVal
eval Nil = return Nil
eval (List []) = return Nil
eval val@(Character _) = return val
eval val@(Integer _) = return val
eval val@(Real _) = return val
eval val@(Rational _) = return val
eval val@(Complex _) = return val
eval val@(Boolean _) = return val
eval val@(Symbol _) = getVariable val
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
eval (List [Symbol "lambda", List formals, body]) = asks (Lambda (Function $ applyLambda body formals))
-- Lambda function of the form (lambda (x y . z) z)
eval (List (Symbol "lambda" : PairList formals vararg : body)) = undefined
-- asks (Lambda (Function $ applyLambda [] body))
-- Lambda function of the form (lambda x x)
eval (List [Symbol "lambda", formal@(Symbol _), body]) = asks (Lambda (Function $ applyLambda body []))
-- Application of functions :D
eval (List ((:) fun exprs)) = do
  func <- eval fun
  val <- mapM eval exprs
  case func of
    (Fun (Function fn)) -> fn val
    (Lambda (Function fn) closure) -> local (const closure) $ fn val
    _ -> throw $ NotFunction func
eval xs = throw (Generic $ "Unknown: " <> showVal xs)

evalBody :: SchemeVal -> Eval SchemeVal
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

applyLambda :: SchemeVal -> [SchemeVal] -> [SchemeVal] -> Eval SchemeVal
applyLambda expr params args = bindArgsEval params args expr

liftThrows :: MonadError e m => Either e a -> m a
liftThrows (Right val) = return val
liftThrows (Left err) = throwError err
