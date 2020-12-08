{-# LANGUAGE OverloadedStrings #-}

module Scheme.ProcedureSpec (spec) where

import Scheme (Number (..), SchemeVal (..))
import Scheme.TestUtils
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "lambda" $ do
    it "(lambda (x) (+ x x))" $ testRunOutput "(lambda (x) (+ x x))" `shouldReturn` "<fun>"
    it "((lambda (x) (+ x x)) 4)" $ testRun "((lambda (x) (+ x x)) 4)" `shouldReturn` Number (Integer 8)
    it "((lambda x x) 3 4 5 6)" $ testRun "((lambda x x) 3 4 5 6)" `shouldReturn` List [Number (Integer 3), Number (Integer 4), Number (Integer 5), Number (Integer 6)]
    it "((lambda (x y . z) z) 3 4 5 6)" $ testRun "((lambda (x y . z) z) 3 4 5 6)" `shouldReturn` List [Number (Integer 5), Number (Integer 6)]
  xdescribe "set!" $ do
    it "(set! x 5)" $ testRun "(set! x 5)" `shouldReturn` Nil
