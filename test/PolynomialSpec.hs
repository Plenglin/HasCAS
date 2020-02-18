module PolynomialSpec where

import Data.Ratio
import Test.Hspec
import TestUtils
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
    it "should scale all terms of a poly" $
      let a = oneMono
          b = mono [("x", 5), ("y", 2)] 
        in sclPolynomial 4 (poly [(a, 4), (b, 2)]) `shouldBe` poly [(a, 16), (b, 8)]
  describe "mulPolynomials" $ do 
    it "should multiply polynomials" $ do
      let x = mono [("x", 1)]
          x2 = mono [("x", 2)] 
        in (poly [(oneMono, 2), (x, 4)]) `mulPolynomials` (poly [(oneMono, 5), (x, 3)]) `shouldBe` poly [(oneMono, 10), (x, 26), (x2, 12)] 
  describe "powPolynomial" $ do
    it "should power" $ do
      let x = mono [("x", 1)]
          x2 = mono [("x", 2)]
        in powPolynomial 2 (poly [(oneMono, 2), (x, 3)]) `shouldBe` poly [(oneMono, 4), (x, 12), (x2, 9)]
  describe "expandPolynomial" $ do
    it "should convert consts" $ 
      expandPolynomial (exprc (-3)) `shouldBe` poly [(oneMono, -3)]
    it "should convert vars" $ 
      expandPolynomial x `shouldBe` poly [(mono [("x", 1)], 1)]
    it "should convert powers" $ do
      expandPolynomial (x ^^^ 5) `shouldBe` poly [(mono [("x", 5)], 1)]
      expandPolynomial (exprc (-2) * (x ^^^ 5)) `shouldBe` poly [(mono [("x", 5)], -2)]
    it "should convert additions" $ do
      expandPolynomial ((x ^^^ 5) + (x ^^^ 2) + (exprc (-3))) `shouldBe` poly [(oneMono, (-3)), (mono [("x", 2)], 1), (mono [("x", 5)], 1)]
    it "should expand and convert expressions" $ do
      expandPolynomial ((exprc (-2)) * ((x ^^^ 5) + (x ^^^ 2) + (exprc (-3)))) `shouldBe` poly [(oneMono, (6)), (mono [("x", 2)], -2), (mono [("x", 5)], -2)]
      let a = exprc 3 * (x ^^^ 5) + (exprc (-2)) * ((x ^^^ 5) + (x ^^^ 2) + (exprc (-3)))
        in expandPolynomial a `shouldBe` poly [(oneMono, 6), (mono [("x", 2)], -2), (mono [("x", 5)], 1)]
    it "should eliminate zero-terms" $ do
      let a = exprc 2 * (x ^^^ 5) + (exprc (-2)) * ((x ^^^ 5) + (x ^^^ 2) + (exprc (-3)))
        in expandPolynomial a `shouldBe` poly [(oneMono, 6), (mono [("x", 2)], -2)]
    it "should expand exponents" $ do
      let a = (x + 1) ^^^ 3
          mx = mono [("x", 1)]
          mx2 = mono [("x", 2)]
          mx3 = mono [("x", 3)]
        in expandPolynomial a `shouldBe` poly [(oneMono, 1), (mx, 3), (mx2, 3), (mx3, 1)]
      
