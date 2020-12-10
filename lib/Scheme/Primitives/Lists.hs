module Scheme.Primitives.Lists
  ( listPrimitives,
  )
where

import Control.Exception (throw)
import Data.Text (Text)
import Scheme.Operators (unOp)
import Scheme.Types (SchemeError (..), SchemeResult, SchemeVal (..))

listPrimitives :: [(Text, [SchemeVal] -> SchemeResult SchemeVal)]
listPrimitives =
  [ ("pair?", unOp isPair),
    ("cons", cons),
    ("car", car),
    ("cdr", cdr),
    ("list?", unOp isList)
  ]

isPair :: SchemeVal -> SchemeVal
isPair (PairList _ _) = Boolean True
isPair (List xs) = Boolean $ not $ null xs
isPair _ = Boolean False

cons :: [SchemeVal] -> SchemeResult SchemeVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x : xs)
cons [x, PairList xs end] = return $ PairList (x : xs) end
cons [x, y] = return $ PairList [x] y
cons xs = throw $ ArgumentLengthMismatch 2 xs

car :: [SchemeVal] -> SchemeResult SchemeVal
car [List (x : _)] = return x
car [PairList (x : _) _] = return x
car [List []] = throw EmptyList
car [PairList [] _] = throw EmptyList
car [x] = throw $ TypeMismatch "list/pair" x
car x = throw $ ArgumentLengthMismatch 1 x

cdr :: [SchemeVal] -> SchemeResult SchemeVal
cdr [List (_ : xs)] = return $ List xs
cdr [PairList [_] x] = return x
cdr [PairList (_ : xs) x] = return $ PairList xs x
cdr [List []] = throw EmptyList
cdr [PairList [] _] = throw EmptyList
cdr [x] = throw $ TypeMismatch "list/pair" x
cdr x = throw $ ArgumentLengthMismatch 1 x

isList :: SchemeVal -> SchemeVal
isList (List _) = Boolean True
isList _ = Boolean False
