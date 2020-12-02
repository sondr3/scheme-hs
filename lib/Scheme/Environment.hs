module Scheme.Environment where

import Control.Monad.Reader (Reader, asks, runReader)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Scheme.Types (SchemeVal (..))

type Environment = Map.Map Text SchemeVal

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
eval _ = undefined
