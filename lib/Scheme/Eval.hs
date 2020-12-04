module Scheme.Eval where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader (ReaderT, ask, asks, runReaderT)
import Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Scheme.Environment (buildEnvironment)
import Scheme.Parser (pExpr)
import Scheme.Types (Environment, SchemeError (..), SchemeVal (..), showError, showVal)
import Text.Megaparsec (errorBundlePretty, runParser)

type Eval a = ReaderT Environment (ExceptT SchemeError Identity) a

scheme :: Text -> Text
scheme input = case runParser pExpr "" input of
  Right val -> execute val
  Left err -> T.pack $ errorBundlePretty err

execute :: SchemeVal -> Text
execute input = case run input of
  Right out -> T.pack $ showVal out
  Left err -> showError err

run :: SchemeVal -> Either SchemeError SchemeVal
run sexp = evaluate buildEnvironment (eval sexp)

evaluate :: Environment -> Eval SchemeVal -> Either SchemeError SchemeVal
evaluate env evl = runIdentity (runExceptT (runReaderT evl env))

eval :: SchemeVal -> Eval SchemeVal
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
