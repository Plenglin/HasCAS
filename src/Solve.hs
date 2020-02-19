module Solve where

import qualified Data.Map.Strict as Map

import Expr

data ExprCompare = Equals | LessOrEqual | GreaterOrEqual | Less | Greater
invSign :: ExprCompare -> ExprCompare
invSign Equals = Equals
invSign LessOrEqual = GreaterOrEqual
invSign GreaterOrEqual = LessOrEqual
invSign Less = Greater
invSign Greater = Less 

newtype Equation = Equation { left :: Expr, comp :: ExprCompare, right :: Expr }

data VarPath = Empty | Found Expr | Parent Expr [VarPath] deriving (Show, Eq)

emptyPath :: VarPath -> Bool
emptyPath Empty = True
emptyPath (Parent _ []) = True
emptyPath _ = False

prunePath :: VarPath -> VarPath
prunePath (Parent x xs) = 
  if null pruned
    then Empty
    else Parent x pruned
  where pruned = filter (not . emptyPath) xs
prunePath x = x

-- | Finds paths to different variable instances
findVar :: String -> Expr -> VarPath
findVar v (A (Var v2))
  | v == v2 = Found (A (Var v2))
findVar v (B a op b) = prunePath (Parent (B a op b) [findVar v a, findVar v b])
findVar v (I op xs) = prunePath (Parent (I op xs) [findVar v x | x <- xs])
findVar v (U op x) = prunePath (Parent (U op x) [findVar v x])
findVar v (Poly pmap) = 
  if or [Map.member v vs | (Monomial vs) <- Map.keys pmap]  -- v is in one of the monomials
    then Found (Poly pmap)
    else Empty
findVar _ _ = Empty

-- 
toUOp :: String -> Expr -> UOp
toUOp var (U op x) = op
