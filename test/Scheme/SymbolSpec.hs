module Scheme.SymbolSpec (spec) where

import Scheme (SchemeVal (..))
import Scheme.TestUtils
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "symbol?" $ do
    it "(symbol? 'foo)" $ testRun "(symbol? 'foo)" `shouldReturn` Boolean True
    it "(symbol? (car '(a b)))" $ testRun "(symbol? (car '(a b)))" `shouldReturn` Boolean True
    it "(symbol? \"bar\")" $ testRun "(symbol? \"bar\")" `shouldReturn` Boolean False
    it "(symbol? 'nil)" $ testRun "(symbol? 'nil)" `shouldReturn` Boolean True
    it "(symbol? '())" $ testRun "(symbol? '())" `shouldReturn` Boolean False
    it "(symbol? #f)" $ testRun "(symbol? #f)" `shouldReturn` Boolean False
  describe "symbol->string" $ do
    it "(symbol->string 'flying-fish)" $ testRun "(symbol->string 'flying-fish)" `shouldReturn` String "flying-fish"
    it "(symbol->string 'Martin)" $ testRun "(symbol->string 'Martin)" `shouldReturn` String "Martin"
    it "(symbol->string (string->symbol \"Malvina\"))" $ testRun "(symbol->string (string->symbol \"Malvina\"))" `shouldReturn` String "Malvina"
  describe "string->symbol" $ do
    it "(string->symbol \"mISSISSIppi\")" $ testRun "(string->symbol \"mISSISSIppi\")" `shouldReturn` Symbol "mISSISSIppi"
    it "(eqv? 'bitBlt (string->symbol \"bitBlt\"))" $ testRun "(eqv? 'bitBlt (string->symbol \"bitBlt\"))" `shouldReturn` Boolean True
    it "(eqv? 'LollyPop(string->symbol(symbol->string 'LollyPop)))" $ testRun "(eqv? 'LollyPop(string->symbol(symbol->string 'LollyPop)))" `shouldReturn` Boolean True
    it "(string=? \"K. Harper, M.D.\" (symbol->string (string->symbol \"K. Harper, M.D.\")))" $ testRun "(string=? \"K. Harper, M.D.\" (symbol->string (string->symbol \"K. Harper, M.D.\")))" `shouldReturn` Boolean True
