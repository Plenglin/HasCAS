import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Lib


a :: Expr
a = fromInteger 5 + ((Var "x") + (fromInteger 2 * fromInteger 5 * fromInteger 2))

main :: IO ()
main = hspec $ do
  describe "evalConstExprs" $ do
    it "does nothing to const" $ do
      evalConstExprs (fromInteger 132) `shouldBe` Const (fromInteger 132)

    it "does nothing to var" $ do
      evalConstExprs (Var "abc") `shouldBe` Var "abc"

    it "reduces a second-level expression" $ do
      evalConstExprs (fromInteger 5 + (Var "x" + (fromInteger 2 * fromInteger 5 * fromInteger 2)))
        `shouldBe` fromInteger 5 + (Var "x" + fromInteger 20)

    it "reduces a multi-level const expression" $ do
      evalConstExprs (fromInteger 5 + (fromInteger 4 - (fromInteger 2 * fromInteger 5 * fromInteger 2)))
        `shouldBe` fromInteger (5 + (4 - (2 * 5 * 2)))
  
  describe "combineConst" $ do
    it "does nothing to Const" $ do
      let a = fromInteger 32 in
        combineConst Add a `shouldBe` a
      
    it "rearranges constants" $ do
      combineConst Add (Var "x" + (fromInteger 3 + Var "z")) 
        `shouldBe`fromInteger 3 + (Var "x" + Var "z")

    it "combines Const expressions" $ do
      combineConst Add (fromInteger 5 + (fromInteger 2 + (fromInteger 3)))
        `shouldBe` fromInteger 10

    it "does nothing to Var expressions" $ do
      let a = Var "x" + (Var "y" + Var "z") in
        combineConst Add a `shouldBe` a

    it "ignores expressions with different ops (1)" $ do
      let a = fromInteger 3 + (fromInteger 5 * fromInteger 9) in
        combineConst Add a `shouldBe` a

    it "ignores expressions with different ops (2)" $ do
      let a = fromInteger 5 * fromInteger 9 in
        combineConst Add a `shouldBe` a

    it "ignores expressions with different ops (3)" $ do
      let a = fromInteger 5 * (fromInteger 9 + fromInteger 2) in
        combineConst Add a `shouldBe` a

