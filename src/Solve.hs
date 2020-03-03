module Solve where

import qualified Data.Map.Strict as Map
import Data.List

import Expr
import TreeUtils

data ExprCompare = Equals | LessOrEqual | GreaterOrEqual | Less | Greater
invSign :: ExprCompare -> ExprCompare
invSign Equals = Equals
invSign LessOrEqual = GreaterOrEqual
invSign GreaterOrEqual = LessOrEqual
invSign Less = Greater
invSign Greater = Less

data Equation = Equation Expr ExprCompare Expr

-- | Represents the results of a search for instances of a variable.
-- | Empty means the variable wasn't found along this path.
-- | Found is the direct instance of the variable.
-- | Compose is a chain of unary functions encasing the variable.
-- | Split means multiple variable instances were found.
data VarPath = Empty | Found Expr | Compose UOp VarPath | Split BOp [VarPath] deriving (Show, Eq)
data PathEquation = PathEquation VarPath ExprCompare VarPath

emptyPath :: VarPath -> Bool
emptyPath Empty = True
emptyPath (Split _ []) = True
emptyPath _ = False

prunePath :: VarPath -> VarPath
prunePath (Split op []) = Empty
prunePath (Split op [x]) = x
prunePath (Split op xs) = Split op (filter (not . emptyPath) xs)
prunePath x = x

-- | Finds paths to every instance of a given variable.
findVar :: String -> Expr -> VarPath
findVar v (A (Var v2))
  | v == v2 = Found (A (Var v2))
findVar v (Poly pmap) =
  if or [Map.member v vs | (Monomial vs) <- Map.keys pmap]  -- v is in one of the monomials
    then Found (Poly pmap)
    else Empty
findVar v (B a op b)  
  | groupable op = findVar v (groupIBO (B a op b))
  | otherwise = 
      case (a', b') of 
        (Empty, Empty) -> Empty
        (Empty, _) -> Compose (LApply a op) b'
        (_, Empty) -> Compose (RApply op b) a'
        _ -> Split op [a', b']
      where a' = findVar v a
            b' = findVar v b
findVar v (I op []) = Empty
findVar v (I op [x]) = findVar v x
findVar v (I op xs) =
  case (vs', ks') of
    ([], _) -> Empty
    ([chain], []) -> chain
    (chains, []) -> prunePath (Split op chains)
    (chains, consts) -> Compose (RApply op (pruneIBO (I op consts))) (prunePath (Split op chains))
  where search = [(x, findVar v x) | x <- xs]
        wasFound (_, Empty) = False
        wasFound _ = True
        (vs, ks) = partition wasFound search
        vs' = map snd vs
        ks' = map fst ks

findVar v (U op x) = Compose op (findVar v x)
findVar _ _ = Empty

data SolutionSet = 

-- | Unapplies Compose until a non-unary operation is found. The first output is a list of
-- | inverted ops, and the second output is the next non-unary operation.
unapplyPath :: VarPath -> ([UOp], VarPath)
unapplyPath (Compose op path) = (invOp op ++ ops, rest)
  where (ops, rest) = unapplyPath path
unapplyPath path = ([], path)

vpToExpr :: VarPath -> Expr
vpToExpr (Found v) = v
vpToExpr (Compose op vp) = U op (vpToExpr vp)
vpToExpr (Split op xs) = I op [vpToExpr x | x <- xs]

-- | 
toUOp :: String -> Expr -> UOp
toUOp var (U op x) = op

