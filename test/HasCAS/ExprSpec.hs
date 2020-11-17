module HasCAS.ExprSpec where

import Test.Hspec
import HasCAS.Expr
import HasCAS.Scalar

spec :: Spec
spec = do
  describe "evaluate" $ do
    it "should succeed on scalars" $
      evaluate (Scl (R 3)) `shouldBe` Right (R 3)
    it "should fail on variables" $
      evaluate (Var "x" :: Expr Z) `shouldBe` Left (Var "x")
    it "should succeed on scalar expressions" $
      evaluate (Add [Scl (Z 3), Scl (Z 8)]) `shouldBe` Right (Z 11)
    it "should reduce, but fail, on variable expressions" $
      evaluate (Mul [Scl (Z 3), Var "x", Scl (Z 8)]) `shouldBe` Left (Add [Scl (Z 24), Var "x"])
    it "should reduce, but fail, on nested variable expressions" $
      evaluate (Mul [Add [Scl (Z 3), Scl (Z 8), Scl (Z 2)], Mul [Add [Var "x", Scl (Z 9), Scl (Z 2)]]]) 
        `shouldBe` Left (Add [Scl (Z 13), Add [Scl (Z 1), Add [Scl (Z 11), Var "x"]]])
