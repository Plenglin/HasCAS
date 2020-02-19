module SolveSpec where

import Test.Hspec

import Lib
import TestUtils


spec :: Spec
spec = 
  describe "findVar" $ do
    it "finds single variables" $ 
      findVar "x" x `shouldBe` Found x
    it "finds vars in binary exprs" $
      findVar "x" (3 + x) `shouldBe` Parent (3 + x) [Found x]
    it "finds vars in expressions with single instance of x" $ do
      findVar "x" (5 * (3 + x)) `shouldBe` Parent (5 * (3 + x)) [Parent (3 + x) [Found x]]
      let a1 = I Add [3, 2, a2, 1]
          a2 = I Mul [a3]
          a3 = x ^^^ 5
      findVar "x" a1 `shouldBe` Parent a1 [Parent a2 [Parent a3 [Found x]]]
      findVar "x" (x + y) `shouldBe` Parent (x + y) [Found x]
    it "finds vars in expressions with multiple instances of x" $ 
      findVar "x" (x + (y + x)) `shouldBe` Parent (x + (y + x)) [Found x, Parent (y + x) [Found x]]
