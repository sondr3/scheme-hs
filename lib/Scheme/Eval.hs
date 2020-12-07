module Scheme.Eval where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader (ReaderT, ask, asks, local, runReaderT)
import Data.Functor.Identity (Identity)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Scheme.Environment (buildEnvironment, createNormalFunc, createVariadicFunc)
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
  Right out -> showVal out
  Left err -> showError err

run :: SchemeVal -> Either SchemeError SchemeVal
run sexp = evaluate buildEnvironment (eval sexp)

evaluate :: Environment -> Eval SchemeVal -> Either SchemeError SchemeVal
evaluate env evl = runIdentity (runExceptT (runReaderT evl env))

eval' :: Environment -> SchemeVal -> Either SchemeError SchemeVal
eval' env val = runIdentity (runExceptT (runReaderT (eval val) env))

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

-- Lambda function of the form (lambda (x y) (+ x y))
eval (List (Symbol "lambda" : List formals : body)) = do
  env <- ask
  liftThrows (createNormalFunc formals body env)
-- Lambda function of the form (lambda (x y . z) z)
eval (List (Symbol "lambda" : PairList formals vararg : body)) = do
  env <- ask
  liftThrows (createVariadicFunc vararg formals body env)
-- Lambda function of the form (lambda x x)
eval (List (Symbol "lambda" : formal@(Symbol _) : body)) = do
  env <- ask
  liftThrows (createVariadicFunc formal [] body env)

-- Definition of the form (define〈variable〉〈expression〉
eval (List (Symbol "define" : var@(Symbol name) : expr)) = do
  env <- ask
  func <- liftThrows $ createVariadicFunc var [] expr env
  undefined
-- (define (〈variable〉 〈formals〉)〈body〉[]
eval (List (Symbol "define" : List (Symbol variable : formals) : body)) = undefined
-- (define (〈variable〉.〈formal〉)〈body〉)
eval (List (Symbol "define" : PairList (Symbol variable : formals) vararg : body)) = undefined
eval (List (fun : args)) = eval fun >>= \f -> mapM eval args >>= apply f
eval xs = throwError (Generic $ "Unknown: " <> showVal xs)

apply :: MonadError SchemeError m => SchemeVal -> [SchemeVal] -> m SchemeVal
apply (PrimitiveFunc fun) args = liftThrows $ fun args
apply (Func _ params vararg body closure) args
  | length params /= length args && isNothing vararg = throwError $ ArgumentMismatch (length params) args
  | otherwise =
    let env = bindVariadic vararg (updateEnv closure)
     in liftThrows $ evalBody env
  where
    bindVariadic arg env = case arg of
      Just a -> Map.union env (Map.fromList [(a, List $ drop (length params) args)])
      Nothing -> env
    updateEnv env = Map.union env (Map.fromList (zip params args))
    evalBody env = last <$> mapM (eval' env) body
apply fun args = throwError (Generic $ "Unknown function: " <> showVal fun <> " " <> T.concat (map showVal args))

liftThrows :: MonadError e m => Either e a -> m a
liftThrows (Right val) = return val
liftThrows (Left err) = throwError err
