module Scheme.ListSpec (spec) where

import Scheme (Number (..), SchemeError (..), SchemeVal (..))
import Scheme.TestUtils
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "pair" $ do
    it "(pair? '(a . b))" $ testRun "(pair? '(a . b))" `shouldReturn` Boolean True
    it "(pair? '(a b c))" $ testRun "(pair? '(a b c))" `shouldReturn` Boolean True
    it "(pair? '())" $ testRun "(pair? '())" `shouldReturn` Boolean False
    it "(pair? '#(a b))" $ testRun "(pair? '#(a b))" `shouldReturn` Boolean False
  describe "cons" $ do
    it "(cons 'a '())" $ testRun "(cons 'a '())" `shouldReturn` List [Symbol "a"]
    it "(cons '(a) '(b c d))" $ testRun "(cons '(a) '(b c d))" `shouldReturn` List [List [Symbol "a"], Symbol "b", Symbol "c", Symbol "d"]
    it "(cons \"a\" '(b c))" $ testRun "(cons \"a\" '(b c))" `shouldReturn` List [String "a", Symbol "b", Symbol "c"]
    it "(cons 'a 3)" $ testRun "(cons 'a 3)" `shouldReturn` PairList [Symbol "a"] (Number (Integer 3))
    it "(cons '(a b) 'c)" $ testRun "(cons '(a b) 'c)" `shouldReturn` PairList [List [Symbol "a", Symbol "b"]] (Symbol "c")
  describe "car" $ do
    it "(car '(a b c))" $ testRun "(car '(a b c))" `shouldReturn` Symbol "a"
    it "(car '((a) b c d))" $ testRun "(car '((a) b c d))" `shouldReturn` List [Symbol "a"]
    it "(car '(1 . 2))" $ testRun "(car '(1 . 2))" `shouldReturn` Number (Integer 1)
    it "(car '())" $ testFail "(car '())" `shouldThrow` isSchemeError
  describe "cdr" $ do
    it "(cdr '((a) b c d))" $ testRun "(cdr '((a) b c d))" `shouldReturn` List [Symbol "b", Symbol "c", Symbol "d"]
    it "(cdr '(1 . 2))" $ testRun "(cdr '(1 . 2))" `shouldReturn` Number (Integer 2)
    it "(cdr '())" $ testFail "(cdr '())" `shouldThrow` isSchemeError
