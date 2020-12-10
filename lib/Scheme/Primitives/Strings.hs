module Scheme.Primitives.Strings
  ( stringPrimitives,
    unwrapString,
  )
where

import Control.Exception (throw)
import Data.Text (Text)
import qualified Data.Text as T
import Scheme.Operators (unOp)
import Scheme.Types (SchemeError (..), SchemeResult, SchemeVal (..))

stringPrimitives :: [(Text, [SchemeVal] -> SchemeResult SchemeVal)]
stringPrimitives =
  [ ("string=?", strBoolOp (==)),
    ("string<?", strBoolOp (<)),
    ("string>?", strBoolOp (>)),
    ("string<=?", strBoolOp (<=)),
    ("string>=?", strBoolOp (>=)),
    ("string-ci=?", strBoolCiOp (==)),
    ("string-ci<?", strBoolCiOp (<)),
    ("string-ci>?", strBoolCiOp (>)),
    ("string-ci<=?", strBoolCiOp (<=)),
    ("string-ci>=?", strBoolCiOp (>=)),
    ("string-upcase", unOp stringUpcase),
    ("string-downcase", unOp stringDowncase),
    ("string-foldcase", unOp stringFoldcase)
  ]

strBoolCiOp :: (Text -> Text -> Bool) -> [SchemeVal] -> Either SchemeError SchemeVal
strBoolCiOp _ [] = throw $ ArgumentLengthMismatch 1 []
strBoolCiOp _ [_] = return $ Boolean True
strBoolCiOp op xs = do
  txts <- map T.toUpper <$> mapM unwrapString xs
  return $ Boolean $ and $ zipWith op txts $ drop 1 txts

strBoolOp :: (Text -> Text -> Bool) -> [SchemeVal] -> Either SchemeError SchemeVal
strBoolOp _ [] = throw $ ArgumentLengthMismatch 1 []
strBoolOp _ [_] = return $ Boolean True
strBoolOp op xs = do
  txts <- mapM unwrapString xs
  return $ Boolean $ and $ zipWith op txts $ drop 1 txts

stringUpcase :: SchemeVal -> SchemeVal
stringUpcase (String s) = String (T.toUpper s)
stringUpcase x = throw $ TypeMismatch "string" x

stringDowncase :: SchemeVal -> SchemeVal
stringDowncase (String s) = String (T.toLower s)
stringDowncase x = throw $ TypeMismatch "string" x

stringFoldcase :: SchemeVal -> SchemeVal
stringFoldcase (String s) = String (T.toCaseFold s)
stringFoldcase x = throw $ TypeMismatch "string" x

unwrapString :: SchemeVal -> SchemeResult Text
unwrapString (String s) = pure s
unwrapString x = throw $ TypeMismatch "string" x
