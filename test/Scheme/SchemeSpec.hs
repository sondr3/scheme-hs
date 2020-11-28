module Scheme.SchemeSpec (spec) where

import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "works" True
