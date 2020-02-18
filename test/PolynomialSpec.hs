module PolynomialSpec where

import Test.Hspec
import Lib

spec :: Spec
spec = do
  describe "pruneMonomial" $
    it "should do nothing to ones" $
      pruneMonomial oneMono `shouldBe` oneMono
