{-# LANGUAGE ExistentialQuantification #-}

module Scheme.Primitives.Equivalence
  ( equivalencePrimitives,
  )
where

import Control.Exception (throw)
import Control.Monad.Except (MonadError (catchError))
import Data.Text (Text)
import Scheme.Primitives.Numbers (unwrapNumber)
import Scheme.Primitives.Strings (unwrapString)
import Scheme.Primitives.Symbols (unwrapSymbol)
import Scheme.Types (SchemeError (..), SchemeResult, SchemeVal (..))

data Unpacker = forall a. Eq a => AnyUnpacker (SchemeVal -> SchemeResult a)

equivalencePrimitives :: [(Text, [SchemeVal] -> SchemeResult SchemeVal)]
equivalencePrimitives =
  [ ("eqv?", eqv),
    ("eq?", eqv),
    ("equal?", equal)
  ]

eqv :: [SchemeVal] -> SchemeResult SchemeVal
eqv [x, y] = return $ Boolean (x == y)
eqv x = throw $ ArgumentLengthMismatch 2 x

unpackEquals :: SchemeVal -> SchemeVal -> Unpacker -> SchemeResult Bool
unpackEquals x y (AnyUnpacker unpacker) =
  do
    u1 <- unpacker x
    u2 <- unpacker y
    return $ u1 == u2
    `catchError` const (return False)

equal :: [SchemeVal] -> SchemeResult SchemeVal
equal [x, y] = do
  primEq <- or <$> mapM (unpackEquals x y) [AnyUnpacker unwrapNumber, AnyUnpacker unwrapString, AnyUnpacker unwrapSymbol]
  eqvEq <- eqv [x, y]
  return $ Boolean (primEq || let (Boolean b) = eqvEq in b)
equal x = throw $ ArgumentLengthMismatch 2 x
