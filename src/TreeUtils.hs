module TreeUtils where

import Data.Ratio
import Data.List
import qualified Data.Map.Strict as Map
import Data.Map.Merge.Strict (zipWithMatched)
import Debug.Trace (trace)

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

groupIBO :: Expr -> Expr
groupIBO (B a op b) = I op (touching op (B a op b))
groupIBO x = x

ungroupIBO :: Expr -> Expr
ungroupIBO (I Mul [A (Const k), A (Var x)]) = exprv x * exprc k
ungroupIBO (I op [a]) = a
ungroupIBO (I op [a, b]) = B a op b
ungroupIBO (I op (x:xs)) = B x op (I op xs)

isConst :: Expr -> Bool
isConst (A (Const _)) = True
isConst _ = False

countVars :: Expr -> Map.Map String Int
countVars x = go [x] Map.empty
  where newVarInst :: Maybe Int -> Maybe Int
        newVarInst (Just n) = Just (n + 1)
        newVarInst Nothing = Just 1

        go :: [Expr] -> Map.Map String Int -> Map.Map String Int
        go [] acc = acc
        go ((B a _ b):stack) acc = go (b:a:stack) acc
        go ((I _ xs):stack) acc = go (xs ++ stack) acc
        go ((A (Var v)):stack) acc = go stack (Map.alter newVarInst v acc)
        go (_:stack) acc = go stack acc

recombineVarCoeff :: BOp -> String -> Scalar -> Expr
recombineVarCoeff op x k
  | identity op == k = exprv x
  | otherwise = B (exprv x) op (exprc k)

combineLikeTerms :: Expr -> Expr
combineLikeTerms (I op exprs) =
  if constSumScl == identity op
    then I op nonConstSum
    else I op (constSum : nonConstSum)
  where
    f = scalarOp op
    ro = repeated op
    i = identity op

    (consts, nonConst) = partition isConst exprs
    combine (A (Const a)) (A (Const b)) = exprc (f a b)
    constSum = foldl combine (exprc i) consts
    A (Const constSumScl) = constSum

    formatCoeff Add (B (A (Const k)) Mul (A (Var x))) = Just (exprv x * exprc k)
    formatCoeff Add (A (Var x)) = Just (exprv x * exprc 1)
    formatCoeff Add (I Mul xs) = Just (ungroupIBO (I Mul xs))
    formatCoeff Mul (A (Var x)) = Just (exprv x ^^^ exprc 1)
    formatCoeff op (B a ro b) = Just (B a ro b)
    formatCoeff _ x = Nothing

    newVarCoeff :: Scalar -> Maybe Scalar -> Maybe Scalar
    newVarCoeff k (Just k1) = Just (k + k1)
    newVarCoeff k Nothing = Just k

    separateVars :: Map.Map String Scalar -> [Expr] -> [Expr] -> (Map.Map String Scalar, [Expr])
    separateVars varsMap nonVars [] = (varsMap, nonVars)
    separateVars varsMap nonVars (expr:rest) =
      case formatCoeff op expr of
        Just (B (A (Var x)) op (A (Const k))) ->
          separateVars (Map.alter (newVarCoeff k) x varsMap) nonVars rest
        _ ->
          separateVars varsMap (expr:nonVars) rest

    (varsMap, nonVars) = separateVars Map.empty [] nonConst
    vars = map (uncurry (recombineVarCoeff ro)) (Map.assocs varsMap)
    nonConstSum = vars ++ nonVars

combineLikeTerms x = x

distribute :: Expr -> Expr
distribute (B (I Add xs) Mul a) = I Add (map (*a) xs)
distribute (B (I Mul xs) Pow a) = I Add (map (^^^a) xs)
distribute x = x

expandPolynomial :: Expr -> Expr
expandPolynomial (B a op b) 
  | op == Add || op == Mul = expandPolynomial (groupIBO (B a op b))
expandPolynomial (I Add xs) = combineLikeTerms (I Add (map expandPolynomial xs))
expandPolynomial x = x
