module Scheme.ParserSpec (spec) where

import Control.Monad (forM_)
import Scheme
import Scheme.TestUtils (testParse)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Printf (printf)

spec :: Spec
spec = parallel $ do
  describe "Parses identifiers" $ do
    let idents = ["...", "++soup+", "<=?", "->string", "a34kTMNs", "lambda", "list->vector", "q", "V17a", "the-word-recursion-has-many-meanings"]

    forM_ idents $ \ident ->
      it (printf "parses %s" ident) $ do
        testParse pSymbol `shouldSucceedOn` ident
