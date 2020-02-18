module PolynomialSpec where

import Data.Ratio
import Test.Hspec
import Lib

spec :: Spec
spec = do
  describe "pruneMonomial" $ do
    it "should do nothing to ones" $
      pruneMonomial oneMono `shouldBe` oneMono
    it "should reduce zero-power terms" $ do
      pruneMonomial (mono [("x", 0), ("y", 2)]) `shouldBe` mono [("y", 2)]
      pruneMonomial (mono [("y", 0), ("z", 0)]) `shouldBe` oneMono
  describe "sclPolynomial" $ do
    context "when scalar = 1" $ 
      it "should do nothing" $ do
        sclPolynomial 1 zeroPoly `shouldBe` zeroPoly
        let a = singlePoly (mono [("x", 5)]) 3 in 
          sclPolynomial 1 a `shouldBe` a
    context "when scalar = 0" $ 
      it "should return zero" $ do
        sclPolynomial 0 zeroPoly `shouldBe` zeroPoly
        sclPolynomial (RaScl (0 % 1)) zeroPoly `shouldBe` zeroPoly
        sclPolynomial 0.0 zeroPoly `shouldBe` zeroPoly

        sclPolynomial 0 (singlePoly (mono [("x", 3)]) 2) `shouldBe` zeroPoly
        sclPolynomial (RaScl (0 % 1)) (singlePoly (mono [("x", 3)]) 2) `shouldBe` zeroPoly
        sclPolynomial (RaScl (0 % 3)) (singlePoly (mono [("x", 3)]) 2) `shouldBe` zeroPoly
        sclPolynomial 0.0 (singlePoly (mono [("x", 3)]) 2) `shouldBe` zeroPoly
