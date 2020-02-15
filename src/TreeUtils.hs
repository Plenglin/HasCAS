module TreeUtils where

import Data.Ratio
import Data.List
import qualified Data.Map.Strict as Map
import Data.Map.Merge.Strict (zipWithMatched)

import Scalar
import Op
import Expr



touching :: BOp -> Expr -> [Expr]
touching op x = go op [x] []
  where go :: BOp -> [Expr] -> [Expr] -> [Expr]
        go op ((B a op2 b):stack) found
          | op == op2 = go op (b:a:stack) found  -- Reverse order to have the result be in order
        go op (x:stack) found = go op stack (x:found)
        go op [] found = found

neg1 :: Expr
neg1 = exprc (-1)

expandInverse :: Expr -> Expr
expandInverse (B a Sub b) = a + (neg1 * b)
expandInverse (B a Div b) = a + B b Pow neg1
expandInverse (U Neg x) = neg1 * x
expandInverse (U Sqrt x) = B x Pow (fromRational (1 % 2))
expandInverse (B a op b) = B (expandInverse a) op (expandInverse b)
expandInverse x = x

toSigma :: Expr -> Expr
toSigma (B a Add b) = S (touching Add (B a Add b))
toSigma x = x

toProd :: Expr -> Expr
toProd (B a Mul b) = P (touching Mul (B a Mul b))
toProd x = x

isConst :: Expr -> Bool
isConst (A (Const _)) = True
isConst _ = False 

involvedVars :: Expr -> Map.Map String Int
involvedVars x = go [x] Map.empty
  where newVarInst :: Maybe Int -> Maybe Int
        newVarInst (Just n) = Just (n + 1)
        newVarInst Nothing = Just 1

        go :: [Expr] -> Map.Map String Int -> Map.Map String Int
        go [] acc = acc
        go ((B a _ b):stack) acc = go (b:a:stack) acc
        go ((S xs):stack) acc = go (xs ++ stack) acc
        go ((A (Var v)):stack) acc = go stack (Map.alter newVarInst v acc)
        go (_:stack) acc = go stack acc

combineLikeTerms :: Expr -> Expr
combineLikeTerms (S xs) = S (constSum : nonConst)
  where (consts, nonConst) = partition isConst xs
        addConst (A (Const a)) (A (Const b)) = A (Const (a + b))
        constSum = foldl addConst 0 consts


reformat :: Expr -> Expr
reformat (B a Add b) = reformat (S (map reformat xs)) 
  where (S xs) = toSigma (B a Add b)
reformat (B a Mul b) = P (map reformat xs)
  where (P xs) = toProd (B a Mul b)
reformat (B a Sub b) = reformat (a + (neg1 * b))
reformat (B a Div b) = reformat (a + (b ^^^ neg1))

reformat (B a op b) = B (reformat a) op (reformat b)
reformat (U op x) = U op (reformat x)
reformat x = expandInverse x
