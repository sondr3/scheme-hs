module Scheme.Environment where

import Control.Monad.Reader (Reader)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Scheme.Types (SchemeVal (..))

type Environment = Map Text SchemeVal

eval :: SchemeVal -> Reader Environment SchemeVal
eval val@(String _) = return val
eval val@(Character _) = return val
eval val@(Number _) = return val
eval val@(Boolean _) = return val
eval (Symbol s) = do
  e <- ask
