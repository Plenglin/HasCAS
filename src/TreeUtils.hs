module TreeUtils(
  reattach,
  straighten
) where


import Op
import Expr
import Scalar

-- | Attaches the second expression tree to the rightmost node of the other. 
reattach :: BOp -> Expr -> Expr -> Expr
reattach op acc (BExpr a op' (BExpr b op'' c))
  | op == op' && op' == op'' = a + (b + (c + acc)) where (+) = exprOp op
reattach op acc (BExpr a op' b)
  | op == op' = a + (b + acc) where (+) = exprOp op
reattach op acc b = exprOp op b acc

-- | Rotates an expression tree to become a straightened tree.
straighten :: BOp -> Expr -> Expr
straighten op (BExpr a op' b)
  | op == op' = reattach op (straighten op a) (straighten op b)
straighten op x = x
