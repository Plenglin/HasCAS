module SolveSpec where

import Test.Hspec

import Lib
import TestUtils


spec :: Spec
spec = 
  describe "findVar" $ do
    it "finds vars in expressions with single instance of x" $ do
      findVar "x" x `shouldBe` [[x]]
      findVar "x" (3 + x) `shouldBe` [[x, (3 + x)]]
      findVar "x" (5 * (3 + x)) `shouldBe` [[x, (3 + x), 5 * (3 + x)]]
      let a1 = I Add [3, 2, a2, 1]
          a2 = I Mul [a3]
          a3 = x ^^^ 5
      findVar "x" a1 `shouldBe` [[x, a3, a2, a1]]
      findVar "x" (x + y) `shouldBe` [[x, x + y]]
    it "finds vars in expressions with multiple instances of x" $ do
      findVar "x" (x + (y + x)) `shouldBe` [[x, x + (y + x)], [x, y + x, x + (y + x)]]
      findVar "x" (I Add [3 - x, 9 + x, 2 ^^^ x]) `shouldBe` []
