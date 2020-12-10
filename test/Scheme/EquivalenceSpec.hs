module Scheme.EquivalenceSpec (spec) where

import Scheme (SchemeVal (..))
import Scheme.TestUtils
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "eqv?" $ do
    it "(eqv? 'a 'a)" $ testRun "(eqv? 'a 'a)" `shouldReturn` Boolean True
    it "(eqv? 'a 'b)" $ testRun "(eqv? 'a 'b)" `shouldReturn` Boolean False
    it "(eqv? 2 2)" $ testRun "(eqv? 2 2)" `shouldReturn` Boolean True
    it "(eqv? 2 2.0)" $ testRun "(eqv? 2 2.0)" `shouldReturn` Boolean False
    it "(eqv? '() '())" $ testRun "(eqv? '() '())" `shouldReturn` Boolean True
    it "(eqv? 100000000 100000000)" $ testRun "(eqv? 100000000 100000000)" `shouldReturn` Boolean True
    it "(eqv? 0.0 +nan.0)" $ testRun "(eqv? 0.0 +nan.0)" `shouldReturn` Boolean False
    it "(eqv? (cons 1 2) (cons 1 2))" $ testRun "(eqv? (cons 1 2) (cons 1 2))" `shouldReturn` Boolean True
    it "(eqv? (lambda () 1) (lambda () 2))" $ testRun "(eqv? (lambda () 1) (lambda () 2))" `shouldReturn` Boolean False
    xit "(let ((p (lambda (x) x)))(eqv? p p))" $ testRun "(let ((p (lambda (x) x)))(eqv? p p))" `shouldReturn` Boolean True
    it "(eqv? #f 'nil)" $ testRun "(eqv? #f 'nil)" `shouldReturn` Boolean False
    it "(eqv? \"\" \"\")" $ testRun "(eqv? \"\" \"\")" `shouldReturn` Boolean True
    it "(eqv? '#() '#())" $ testRun "(eqv? '#() '#())" `shouldReturn` Boolean True
    it "(eqv? (lambda (x) x) (lambda (x) x))" $ testRun "(eqv? (lambda (x) x) (lambda (x) x))" `shouldReturn` Boolean False
    it "(eqv? (lambda (x) x) (lambda (y) y))" $ testRun "(eqv? (lambda (x) x) (lambda (y) y))" `shouldReturn` Boolean False
    it "(eqv? +nan.0 +nan.0)" $ testRun "(eqv? +nan.0 +nan.0)" `shouldReturn` Boolean False
  describe "equal?" $ do
    it "(equal? 'a 'a)" $ testRun "(equal? 'a 'a)" `shouldReturn` Boolean True
    it "(equal? '(a) '(a))" $ testRun "(equal? '(a) '(a))" `shouldReturn` Boolean True
    it "(equal? '(a (b) c)'(a (b) c))" $ testRun "(equal? '(a (b) c)'(a (b) c))" `shouldReturn` Boolean True
    it "(equal? \"abc\" \"abc\")" $ testRun "(equal? \"abc\" \"abc\")" `shouldReturn` Boolean True
    it "(equal? 2 2)" $ testRun "(equal? 2 2)" `shouldReturn` Boolean True
    xit "(equal? (make-vector 5 'a)(make-vector 5 'a))" $ testRun "(equal? (make-vector 5 'a)(make-vector 5 'a))" `shouldReturn` Boolean True
    it "(equal? (lambda (x) x)(lambda (y) y))" $ testRun "(equal? (lambda (x) x)(lambda (y) y))" `shouldReturn` Boolean False
