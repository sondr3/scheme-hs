module Scheme.Primitives.Boolean
  ( booleanPrimitives,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Text (Text)
import Scheme.Operators (unOp)
import Scheme.Types (SchemeError (..), SchemeResult, SchemeVal (..))

booleanPrimitives :: [(Text, [SchemeVal] -> SchemeResult SchemeVal)]
booleanPrimitives =
  [ ("and", booleanBoolOp (&&)),
    ("or", booleanBoolOp (||)),
    ("boolean?", unOp isBoolean),
    ("boolean=?", booleanEq)
  ]

isBoolean :: SchemeVal -> SchemeVal
isBoolean (Boolean _) = Boolean True
isBoolean _ = Boolean False

booleanEq :: [SchemeVal] -> SchemeResult SchemeVal
booleanEq [] = throwError $ ArgumentLengthMismatch 1 []
booleanEq [x] = Boolean <$> unwrapBoolean x
booleanEq xs = do
  bools <- mapM unwrapBoolean xs
  return $ Boolean $ and $ zipWith (==) bools $ drop 1 bools

booleanBoolOp :: (Bool -> Bool -> Bool) -> [SchemeVal] -> SchemeResult SchemeVal
booleanBoolOp _ [] = throwError $ ArgumentLengthMismatch 2 []
booleanBoolOp _ [x] = throwError $ ArgumentLengthMismatch 2 [x]
booleanBoolOp op xs = do
  bools <- mapM unwrapBoolean xs
  return $ Boolean $ foldl1 op bools

unwrapBoolean :: SchemeVal -> SchemeResult Bool
unwrapBoolean (Boolean x) = pure x
unwrapBoolean x = throwError $ TypeMismatch "boolean" x
