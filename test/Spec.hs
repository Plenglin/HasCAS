import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Ratio

import Lib


w = exprv "w"
x = exprv "x"
y = exprv "y"
z = exprv "z"

main :: IO ()
main = hspec $ do
  describe "touching" $ do
    it "wraps single atoms in a list" $ do
      touching Add x `shouldBe` [x]
    it "groups single-level B's" $ do
      touching Add (x + y) `shouldBe` [x, y]
    it "groups linear B's" $ do
      touching Add (x + (y + z)) `shouldBe` [x, y, z]
    it "groups balanced B's" $ do
      touching Add ((w + x) + (y + z)) `shouldBe` [w, x, y, z]
    it "ignores different op B's (1)" $ do
      touching Add ((w * x) + (y * z)) `shouldBe` [w * x, y * z]
    it "ignores different op B's (1)" $ do
      touching Add (z + (w + (y * z))) `shouldBe` [z, w, y * z]
    it "ignores different op B's (2)" $ do
      let a = (w * x) * (y * z) in
        touching Add a `shouldBe` [a]
  
  describe "expandInverse" $ do
    it "expands unary negation" $ do
      expandInverse (-x) `shouldBe` ((exprc (-1)) * x)
    it "expands subtraction" $ do
      expandInverse (y - x) `shouldBe` y + ((exprc (-1)) * x)
    it "expands division" $ do
      expandInverse (y / x) `shouldBe` y + (B x Pow (exprc (-1)))
    it "expands sqrt" $ do
      expandInverse (U Sqrt x) `shouldBe` B x Pow (exprc (fromRational (1 % 2)))

  describe "toSigma" $ do
    it "sums multiple terms" $ do
      toSigma (x + y + z) `shouldBe` S [x, y, z]
    it "doesn't sum single terms" $ do
      toSigma x `shouldBe` x
    it "doesn't convert non-Add expressions" $ do
      toSigma (x * y) `shouldBe` x * y

  describe "toProd" $ do
    it "multiply multiple terms" $ do
      toProd (x * y * z) `shouldBe` P [x, y, z]
    it "doesn't multiply single terms" $ do
      toProd x `shouldBe` x
    it "doesn't convert non-Mul expressions" $ do
      toProd (x + y) `shouldBe` x + y
