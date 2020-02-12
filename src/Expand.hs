module Expand(
  expandInverse
) where

import Expr
import Op
import Scalar


neg1 = Const (fromInteger (-1))

-- | Recursively converts Sub and Div to equivalent Add and Mul counterparts
expandInverse :: Expr -> Expr
expandInverse (BExpr a Sub b) = BExpr a Add (BExpr neg1 Mul b)
expandInverse (BExpr a Div b) = BExpr a Mul (BExpr b Pow neg1)
expandInverse (BExpr a op b) = BExpr (expandInverse a) op (expandInverse b)
expandInverse x = x

-- distributeHelper :: BOp -> BOp -> Expr -> [Expr] -> Expr -> [Expr]
-- distributeHelper o1 o2 a terms (BExpr b o3 c)
  -- | o2 == o3 = distributeHelper o1 o2 a (BExpr a o1 b : terms) c
-- distributeHelper o1 o2 a terms x = BExpr a o1 x

-- | Let o1 be the first op and o2 be the second op.
-- | distribute converts an expression of the form a o1 (b o2 c o2 d o2) into
-- | (a o1 b) o2 (a o1 c) o2 (a o1 d)
-- distribute :: BOp -> BOp -> Expr -> Expr
-- distribute o1 o2 (BExpr a o3 (BExpr b o4 c)) 
  -- | o1 == o3 && o2 == o4 = 
