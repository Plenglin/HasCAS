module TreeUtils where


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

combineConstHelper :: BOp -> Scalar -> [Expr] -> Expr -> (Scalar, [Expr])
combineConstHelper op scl terms (BExpr a op2 b)
  | op == op2 =
      case a of
        Const a' -> combineConstHelper op (fs a' scl) terms b
        _ -> combineConstHelper op scl (a:terms) b
    where fe = exprOp op
          fs = scalarOp op
combineConstHelper op scl terms (Const a) = (scalarOp op a scl, terms)
combineConstHelper op scl terms x = (scl, x:terms)

-- | Given a straightened expression tree, combines constants under the top-level op.
combineConst :: BOp -> Expr -> Expr
combineConst op (BExpr a op2 b) 
  | op == op2 = 
      if scl == identity op 
        then combinedTerms
        else exprOp op (Const scl) combinedTerms
    where (scl, terms) = combineConstHelper op (identity op) [] (BExpr a op b)
          combinedTerms = 
            case terms of
              [] -> Id
              t:ts -> foldl (\y x -> exprOp op x y) t ts

combineConst op x = x

-- | Given a straightened, const-less expression tree, combines single variables
--combineVars :: BOp -> Expr -> Expr

