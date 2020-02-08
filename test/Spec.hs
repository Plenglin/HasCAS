import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Lib


a :: Expr
a = fromInteger 5 + ((Var "x") + (fromInteger 2 * fromInteger 5 * fromInteger 2))

main :: IO ()
main = hspec $ do
  describe "evalConstExprs" $ do
    it "does nothing to Const" $ do
      evalConstExprs (fromInteger 132) `shouldBe` Const (fromInteger 132)

    it "does nothing to Var" $ do
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

    it "does nothing to Var expressions" $ do
      let a = Var "x" + (Var "y" + Var "z") in
        combineConst Add a `shouldBe` a

    it "does nothing to expressions with different ops (1)" $ do
      let a = fromInteger 3 + (fromInteger 5 * fromInteger 9) in
        combineConst Add a `shouldBe` a

    it "does nothing to expressions with different ops (2)" $ do
      let a = fromInteger 5 * fromInteger 9 in
        combineConst Add a `shouldBe` a

    it "does nothing to expressions with different ops (3)" $ do
      let a = fromInteger 5 * (fromInteger 9 + fromInteger 2) in
        combineConst Add a `shouldBe` a

    it "does nothing to Const on the right" $ do
      let a = fromInteger 5 * Var "x" in
        combineConst Add a `shouldBe` a
        
    it "swaps Const to the left" $ do
      combineConst Add (Var "x" + fromInteger 3) `shouldBe` (fromInteger 3 + Var "x")
      
    it "extracts consts from inner left" $ do
      combineConst Add (Var "x" + (fromInteger 3 + Var "z")) 
        `shouldBe` fromInteger 3 + (Var "x" + Var "z")

    it "extracts consts from inner right" $ do
      combineConst Add (Var "x" + (Var "z" + fromInteger 3)) 
        `shouldBe` fromInteger 3 + (Var "x" + Var "z")
    
    it "extracts deeper consts" $ do
      combineConst Add (Var "x" + (Var "z" + (Var "y" + fromInteger 3)))
        `shouldBe` fromInteger 3 + (Var "x" + (Var "z" + Var "y"))
    
    it "combines 2-Const, 1-Var expressions (1)" $ do
      combineConst Add (Var "x" + (fromInteger 1 + fromInteger 1))
        `shouldBe` fromInteger 2 + Var "x"
    
    it "combines 2-Const, 1-Var expressions (2)" $ do
      combineConst Add (fromInteger 1 + (Var "x" + fromInteger 1))
        `shouldBe` fromInteger 2 + Var "x"
    
    it "combines 2-Const, 1-Var expressions (3)" $ do
      combineConst Add (fromInteger 1 + (fromInteger 1 + Var "x"))
        `shouldBe` fromInteger 2 + Var "x"
    
    it "combines 3-Const expressions" $ do
      combineConst Add (fromInteger 5 + (fromInteger 2 + fromInteger 3))
        `shouldBe` fromInteger 10

    it "combines 4-Const expressions" $ do
      combineConst Add (fromInteger 1 + (fromInteger 2 + (fromInteger 3 + (fromInteger 4))))
        `shouldBe` fromInteger 10

    it "combines 5-Const expressions" $ do
      combineConst Add (fromInteger 1 + (fromInteger 2 + (fromInteger 3 + (fromInteger 4 + (fromInteger 5)))))
        `shouldBe` fromInteger 15

    it "reduces expressions with deeper constants (1)" $ do
      combineConst Add (fromInteger 1 + (Var "x" + (fromInteger 2 + fromInteger 3)))
        `shouldBe` fromInteger 6 + Var "x"

    it "reduces expressions with deeper constants (2)" $ do
      combineConst Add (fromInteger 1 + (Var "x" + (Var "y" + fromInteger 3)))
        `shouldBe` fromInteger 4 + (Var "x" + Var "y")
