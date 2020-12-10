module Scheme.Primitives.Symbols
  ( symbolPrimitives,
    unwrapSymbol,
  )
where

import Control.Exception (throw)
import Control.Monad.Except (MonadError (throwError))
import Data.Text (Text)
import Scheme.Operators (unOp)
import Scheme.Types (SchemeError (..), SchemeResult, SchemeVal (..))

symbolPrimitives :: [(Text, [SchemeVal] -> SchemeResult SchemeVal)]
symbolPrimitives =
  [ ("symbol?", unOp isSymbol),
    ("symbol=?", symbolEq),
    ("symbol->string", unOp symbolToString),
    ("string->symbol", unOp stringToSymbol)
  ]

isSymbol :: SchemeVal -> SchemeVal
isSymbol (Symbol _) = Boolean True
isSymbol _ = Boolean False

symbolEq :: [SchemeVal] -> SchemeResult SchemeVal
symbolEq [] = throw $ ArgumentLengthMismatch 1 []
symbolEq [Symbol _] = return $ Boolean True
symbolEq xs = do
  syms <- mapM unwrapSymbol xs
  return $ Boolean $ and $ zipWith (==) syms $ drop 1 syms

symbolToString :: SchemeVal -> SchemeVal
symbolToString (Symbol s) = String s
symbolToString x = throw $ TypeMismatch "symbol" x

stringToSymbol :: SchemeVal -> SchemeVal
stringToSymbol (String s) = Symbol s
stringToSymbol x = throw $ TypeMismatch "symbol" x

unwrapSymbol :: SchemeVal -> SchemeResult Text
unwrapSymbol (Symbol s) = pure s
unwrapSymbol x = throwError $ TypeMismatch "symbol" x
