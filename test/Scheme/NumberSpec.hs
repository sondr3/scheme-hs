module Scheme.NumberSpec (spec) where

import Scheme (SchemeVal (..))
import Scheme.TestUtils
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "number type" $ do
    it "(complex? 3.0+4i)" $ testRun "(complex? 3.0+4i)" `shouldBe` Boolean True
    it "(complex? 3)" $ testRun "(complex? 3)" `shouldBe` Boolean True
    it "(real? 3)" $ testRun "(real? 3)" `shouldBe` Boolean True
    it "(real? -2.5+0i)" $ testRun "(real? -2.5+0i)" `shouldBe` Boolean True
    it "(real? -2.5+0.0i)" $ testRun "(real? -2.5+0.0i)" `shouldBe` Boolean True
    it "(real? #e1e10)" $ testRun "(real? #e1e10)" `shouldBe` Boolean True
    it "(real? +inf.0)" $ testRun "(real? +inf.0)" `shouldBe` Boolean True
    it "(real? +nan.0)" $ testRun "(real? +nan.0)" `shouldBe` Boolean True
    it "(rational? -inf.0)" $ testRun "(rational? -inf.0)" `shouldBe` Boolean False
    it "(rational? 3.5)" $ testRun "(rational? 3.5)" `shouldBe` Boolean True
    it "(rational? 6/10)" $ testRun "(rational? 6/10)" `shouldBe` Boolean True
    it "(rational? 6/3)" $ testRun "(rational? 6/3)" `shouldBe` Boolean True
    it "(integer? 3+0i)" $ testRun "(integer? 3+0i)" `shouldBe` Boolean True
    it "(integer? 3.0)" $ testRun "(integer? 3.0)" `shouldBe` Boolean True
    it "(integer? 8/4)" $ testRun "(integer? 8/4)" `shouldBe` Boolean True
  describe "exact?/inexact?" $ do
    it "(exact? 3.0)" $ testRun "(exact? 3.0)" `shouldBe` Boolean False
    it "(exact? #e3.0)" $ testRun "(exact? #e3.0)" `shouldBe` Boolean True
    it "(inexact? 3.0)" $ testRun "(inexact? 3.0)" `shouldBe` Boolean True
  describe "exact-integer?" $ do
    it "(exact-integer? 32)" $ testRun "(exact-integer? 32)" `shouldBe` Boolean True
    it "(exact-integer? 32.0)" $ testRun "(exact-integer? 32.0)" `shouldBe` Boolean False
    it "(exact-integer? 32/5)" $ testRun "(exact-integer? 32/5)" `shouldBe` Boolean False
  describe "finite?" $ do
    it "(finite? 3)" $ testRun "(finite? 3)" `shouldBe` Boolean True
    it "(finite? +inf.0)" $ testRun "(finite? +inf.0)" `shouldBe` Boolean False
    it "(finite? 3.0+inf.0i)" $ testRun "(finite? 3.0+inf.0i)" `shouldBe` Boolean False
  describe "nan?" $ do
    it "(nan? +nan.0)" $ testRun "(nan? +nan.0)" `shouldBe` Boolean True
    it "(nan? 32)" $ testRun "(nan? 32)" `shouldBe` Boolean False
    it "(nan? +nan.0+5.0i)" $ testRun "(nan? +nan.0+5.0i)" `shouldBe` Boolean True
    it "(nan? 1+2i)" $ testRun "(nan? 1+2i)" `shouldBe` Boolean False
