module TreeUtils where


import Expr

-- | Attaches the second expression tree to the rightmost node of the other. 
reattach :: BOp -> Expr -> Expr -> Expr
reattach op acc (BExpr a op' (BExpr b op'' c))
  | op == op' && op' == op'' = a + (b + (c + acc)) where (+) = exprOp op
reattach op acc (BExpr a op' b) 
  | op == op' = a + (b + acc) where (+) = exprOp op
reattach op acc b = exprOp op b acc

-- | Rotates an expression tree to become a straightened tree.
reorient :: BOp -> Expr -> Expr
reorient op (BExpr a op' b) 
  | op == op' = reattach op (reorient op a) (reorient op b)
reorient op x = x

-- | Evaluates all expressions involving constants in the expression tree.
evalConstExprs :: Expr -> Expr
evalConstExprs (BExpr (Const a) op (Const b)) = Const (scalarOp op a b)
evalConstExprs (UExpr op a) = UExpr op (evalConstExprs a)

evalConstExprs (BExpr (Const a) op b) =
  case evalConstExprs b of
    Const b' -> Const (scalarOp op a b')
    b' -> BExpr (Const a) op b'

evalConstExprs (BExpr a op (Const b)) = 
  case evalConstExprs a of
    Const a' -> Const (scalarOp op a' b)
    a' -> BExpr a' op (Const b)

evalConstExprs (BExpr a op b) = BExpr (evalConstExprs a) op (evalConstExprs b)
evalConstExprs x = x

-- | Given an expression tree that has been straightened, combines constants under the operation op.
{-combineConstantsHelper :: BOp -> Expr -> Expr
combineConstantsHelper op (BExpr (Const a) op2 (BExpr (Const b) op3 c))
  | op == op2 && op2 == op3 = BExpr (Const (scalarExpr op a b)) op c'
    where c' = combineConstantsHelper op c

combineConstantsHelper op (BExpr (Const a) op2 (BExpr b op3 c))
  | op == op2 && op2 == op3 = BExpr (Const a) op c' 
    where c' = combineConstantsHelper op c

combineConstantsHelper op a b = BExpr op a b
-}