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
straighten :: BOp -> Expr -> Expr
straighten op (BExpr a op' b) 
  | op == op' = reattach op (straighten op a) (straighten op b)
straighten op x = x

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
combineConst :: BOp -> Expr -> Expr
combineConst op (BExpr (Const a) op2 (Const b))
  | op == op2 = Const (scalarOp op a b)

combineConst op (BExpr (Const a) op2 (BExpr (Const b) op3 c))
  | op == op2 && op2 == op3 = 
    case combineConst op c of
      Const c' -> Const (fs a (fs b c'))
      c' -> fe (Const a) (fe (Const b) c')
    where fs = scalarOp op
          fe = exprOp op

combineConst op (BExpr (Const a) op2 (BExpr b op3 c))
  | op == op2 && op2 == op3 = 
    case combineConst op c of
      Const c' -> fe (Const (fs a c')) b
      c' -> fe (Const a) (fe b c')
    where fs = scalarOp op
          fe = exprOp op

combineConst op (BExpr a op2 (BExpr (Const b) op3 c))
  | op == op2 && op2 == op3 = 
    case combineConst op c of
      Const c' -> BExpr (Const (fs b c')) op a
      c' -> BExpr (Const b) op (fe a c')
    where fs = scalarOp op
          fe = exprOp op

combineConst op x = x