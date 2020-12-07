{-# LANGUAGE OverloadedStrings #-}

module Scheme.ProcedureSpec (spec) where

import Scheme (SchemeVal (..))
import Scheme.TestUtils
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "lambda" $ do
    it "(lambda (x) (+ x x))" $ testRunOutput "(lambda (x) (+ x x))" `shouldBe` "<lambda>"
    it "((lambda (x) (+ x x)) 4)" $ testRun "((lambda (x) (+ x x)) 4)" `shouldBe` Integer 8
    it "((lambda x x) 3 4 5 6)" $ testRun "((lambda x x) 3 4 5 6)" `shouldBe` List [Integer 3, Integer 4, Integer 5, Integer 6]
    it "((lambda (x y . z) z) 3 4 5 6)" $ testRun "((lambda (x y . z) z) 3 4 5 6)" `shouldBe` List [Integer 5, Integer 6]
