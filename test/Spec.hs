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
