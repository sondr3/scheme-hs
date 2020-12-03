module Scheme.Primitives.Numbers
  ( numberPrimitives,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Foldable (foldlM)
import Data.Text (Text)
import Scheme.Types (SchemeError (..), SchemeVal (..), castNum)

numberPrimitives :: [(Text, [SchemeVal] -> Either SchemeError SchemeVal)]
numberPrimitives = [("+", add)]

foldl1M :: Monad m => (a -> a -> m a) -> [a] -> m a
foldl1M f (x : xs) = foldlM f x xs
foldl1M _ [] = error "empty argument list"

add :: [SchemeVal] -> Either SchemeError SchemeVal
add = foldl1M (\x y -> castNum [x, y] >>= add')
  where
    add' (List [Integer x, Integer y]) = pure $ Integer (x + y)
    add' (List [Real x, Real y]) = pure $ Real (x + y)
    add' (List [Rational x, Rational y]) = pure $ Rational (x + y)
    add' (List [Complex x, Complex y]) = pure $ Complex (x + y)
    add' _ = throwError $ Generic "no u"
