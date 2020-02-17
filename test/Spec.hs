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

  describe "groupIBO" $ do
    it "groups multiple terms" $ do
      groupIBO (x + y + z) `shouldBe` I Add [x, y, z]
      groupIBO (x * y * z) `shouldBe` I Mul [x, y, z]
    it "doesn't group single terms" $
      groupIBO x `shouldBe` x

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

  describe "countVars" $ do
    it "works on Var" $ do
      countVars x `shouldBe` Map.singleton "x" 1
      countVars y `shouldBe` Map.singleton "y" 1
    it "is empty on Const" $
      countVars (exprc 3) `shouldBe` Map.empty
    it "works on sums" $
      countVars (eS [x, 1, 3]) `shouldBe` Map.singleton "x" 1
    it "works on nested" $
      countVars (eS [(x - x) + (y + (z + w)), 1, 3]) 
        `shouldBe` Map.fromAscList [("w", 1), ("x", 2), ("y", 1), ("z", 1)]

  --describe "reformat" $
  --  it "converts Add into Sigma, Mul into Prod" $ do
  --    reformat ((x + y) * (x + y + z)) `shouldBe` eP [eS [x, y], eS [x, y, z]]
  --    reformat ((x + y) * (x + y * (w + x + y + z))) `shouldBe` eP [eS [x, y], eS [x, eP [y, eS[w, x, y, z]]]]

  describe "combineLikeTerms" $ do
    it "combines constants" $ do
      combineLikeTerms (I Add [3, 5, 7]) `shouldBe` I Add [exprc (3 + 5 + 7)]
      combineLikeTerms (I Mul [5, 9, 2]) `shouldBe` I Mul [exprc (5 * 9 * 2)]
      combineLikeTerms (I Add [5]) `shouldBe` I Add [exprc 5]
    it "combines vars" $ do
      combineLikeTerms (I Add [x, x, x]) `shouldBe` I Add [x * exprc 3]
      combineLikeTerms (I Add [x, 3 * x, x * 5]) `shouldBe` I Add [x * exprc 9]
      combineLikeTerms (I Add [y, x * 2, x * 93, y * exprc (-93)]) `shouldBe` I Add [x * exprc 95, y * exprc (-92)]
      combineLikeTerms (I Add [x, y, z]) `shouldBe` I Add [x, y, z]
      combineLikeTerms (I Add [z, y, x * 3]) `shouldBe` I Add [x * 3, y, z]
    it "combines constants and vars" $ do
      combineLikeTerms (I Add [x, 3, x * exprc (-3), 5, x]) `shouldBe` I Add [8, x * exprc (-1)]
      combineLikeTerms (I Add [y, x, x]) `shouldBe` I Add [x * exprc 2, y]
      combineLikeTerms (I Add [z, y, x]) `shouldBe` I Add [x, y, z]
      combineLikeTerms (I Add [x, y, z]) `shouldBe` I Add [x, y, z]
    it "combines constants and vars with powers" $ do
      combineLikeTerms (I Mul [y, 3, x ^^^ exprc (-3), 5, y, x ^^^ exprc 22]) `shouldBe` I Mul [15, x ^^^ exprc 19, y ^^^ 2]
      combineLikeTerms (I Mul [x ^^^ exprc (-3), x ^^^ exprc 66]) `shouldBe` I Mul [x ^^^ exprc 63]

  describe "liftAssociative" $ do 
    it "lifts nested I Adds" $ do
      liftAssociative (I Add [w, x, I Add [y, z]]) `shouldBe` I Add [w, x, y, z]
      liftAssociative (I Add [w, x, I Add [y, I Add [z, w]]]) `shouldBe` I Add [w, x, y, z, w]
    it "lifts nested Adds of any type" $ do
      liftAssociative (I Add [w, x, y + z]) `shouldBe` I Add [w, x, y, z]
      liftAssociative (I Add [w, x, y + (x + x)]) `shouldBe` I Add [w, x, y, x, x]
    it "doesn't combine Adds with Muls" $ do
      liftAssociative (I Add [w, x, y * z]) `shouldBe` I Add [w, x, I Mul [y, z]]
      liftAssociative (I Add [w, x, y * (z + (y * w) + x)]) `shouldBe` I Add [w, x, I Mul [y, I Add [z, I Mul [y, w], x]]]
