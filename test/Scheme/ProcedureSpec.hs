{-# LANGUAGE OverloadedStrings #-}

module Scheme.ProcedureSpec (spec) where

import Scheme (Number (..), SchemeVal (..))
import Scheme.TestUtils
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "lambda" $ do
    it "(lambda (x) (+ x x))" $ testRunOutput "(lambda (x) (+ x x))" `shouldBe` "<fun>"
    it "((lambda (x) (+ x x)) 4)" $ testRun "((lambda (x) (+ x x)) 4)" `shouldBe` Number (Integer 8)
    it "((lambda x x) 3 4 5 6)" $ testRun "((lambda x x) 3 4 5 6)" `shouldBe` List [Number (Integer 3), Number (Integer 4), Number (Integer 5), Number (Integer 6)]
    it "((lambda (x y . z) z) 3 4 5 6)" $ testRun "((lambda (x y . z) z) 3 4 5 6)" `shouldBe` List [Number (Integer 5), Number (Integer 6)]
