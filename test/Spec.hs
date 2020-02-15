import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Ratio
import qualified Data.Map as Map

import Lib


w = exprv "w"
x = exprv "x"
y = exprv "y"
z = exprv "z"

main :: IO ()
main = hspec $ do
  describe "touching" $ do
    it "wraps single atoms in a list" $
      touching Add x `shouldBe` [x]
    it "groups single-level B's" $
      touching Add (x + y) `shouldBe` [x, y]
    it "groups linear B's" $
      touching Add (x + (y + z)) `shouldBe` [x, y, z]
    it "groups balanced B's" $ 
      touching Add ((w + x) + (y + z)) `shouldBe` [w, x, y, z]
    it "ignores different op B's" $ do
      touching Add ((w * x) + (y * z)) `shouldBe` [w * x, y * z]
      touching Add (z + (w + (y * z))) `shouldBe` [z, w, y * z]
      let a = (w * x) * (y * z) in
        touching Add a `shouldBe` [a]

  describe "expandInverse" $ do
    it "expands unary negation" $
      expandInverse (-x) `shouldBe` ((exprc (-1)) * x)
    it "expands subtraction" $
      expandInverse (y - x) `shouldBe` y + ((exprc (-1)) * x)
    it "expands division" $
      expandInverse (y / x) `shouldBe` y + (B x Pow (exprc (-1)))
    it "expands sqrt" $
      expandInverse (U Sqrt x) `shouldBe` B x Pow (exprc (fromRational (1 % 2)))

  describe "toSigma" $ do
    it "groups multiple terms" $ 
      toSigma (x + y + z) `shouldBe` S [x, y, z]
    it "doesn't sum single terms" $
      toSigma x `shouldBe` x
    it "doesn't convert non-Add expressions" $ 
      toSigma (x * y) `shouldBe` x * y

  describe "toProd" $ do
    it "groups multiple terms" $ 
      toProd (x * y * z) `shouldBe` P [x, y, z]
    it "doesn't multiply single terms" $ 
      toProd x `shouldBe` x
    it "doesn't convert non-Mul expressions" $ 
      toProd (x + y) `shouldBe` x + y

  describe "Expr ordering" $ do
    it "compares consts by their contents" $ do
      compare (A (Const 5)) (A (Const 5)) `shouldBe` EQ
      compare (A (Const 5)) (A (Const 5.0)) `shouldBe` EQ
      compare (A (Const 5)) (A (Const 9)) `shouldBe` LT
      compare (A (Const 32)) (A (Const 5.0)) `shouldBe` GT
    it "compares vars lexicographically" $ do
      compare x x `shouldBe` EQ
      compare x z `shouldBe` LT
      compare z w `shouldBe` GT
    it "always puts consts before vars" $ do
      compare (A (Const 3)) x `shouldBe` LT
      compare x (A (Const 2)) `shouldBe` GT

  describe "involvedVars" $ do
    it "works on Var" $ do
      involvedVars x `shouldBe` Map.singleton "x" 1
      involvedVars y `shouldBe` Map.singleton "y" 1
    it "is empty on Const" $ do
      involvedVars (exprc 3) `shouldBe` Map.empty

  describe "reformat" $
    it "converts Add into Sigma, Mul into Prod" $ do
      reformat ((x + y) * (x + y + z)) `shouldBe` P [S [x, y], S [x, y, z]]
      reformat ((x + y) * (x + y * (w + x + y + z))) `shouldBe` P [S [x, y], S [x, P [y, S[w, x, y, z]]]]
