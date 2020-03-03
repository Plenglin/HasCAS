module SolveSpec where

import Test.Hspec

import Lib
import TestUtils


spec :: Spec
spec = do
  describe "findVar" $ do
    it "finds single variables" $ 
      findVar "x" x `shouldBe` Found x
    it "finds vars in binary exprs" $ do
      findVar "x" (x ^^^ 3) `shouldBe` Compose (RApply Pow 3) (Found x)
      findVar "x" (3 ^^^ x) `shouldBe` Compose (LApply 3 Pow) (Found x)
    it "RApply's commutative ops" $ do
      findVar "x" (5 * (3 + x)) `shouldBe` Compose (RApply Mul 5) (Compose (RApply Add 3) (Found x))
      let a1 = I Add [3, 2, a2, 1]
          a2 = I Mul [a3]
          a3 = x ^^^ 5
      findVar "x" a1 `shouldBe` Compose (RApply Add (I Add [3, 2, 1])) (Compose (RApply Pow 5) (Found x))
      findVar "x" (x + y) `shouldBe` Compose (RApply Add y) (Found x)
    it "use Split in expressions with multiple instances of x" $ 
      findVar "x" (x + (y ^^^ x)) `shouldBe` Split Add [Found x, Compose (LApply y Pow) (Found x)]
  describe "unapplyPath" $ do
    it "returns empty list on single vars" $
      unapplyPath (Found x) `shouldBe` ([], Found x)
    it "unapplies single compositions" $
      unapplyPath (Compose (RApply Pow 3) (Found x)) `shouldBe` ([RApply Pow (exprc 1 / exprc 3)], Found x)
    it "unapplies multiple compositions" $
      unapplyPath (Compose Sin (Compose Cos (Found x))) `shouldBe` ([ArcSin, ArcCos], Found x)
