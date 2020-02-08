module Expr where

import Debug.Trace
import Scalar

data BOp = Add | Mul | Sub | Div | Pow deriving Eq
data UOp = Abs | Sign deriving Show

instance Show BOp where
  show Add = "+"
  show Mul = "*"
  show Div = "/"
  show Sub = "-"
  show Pow = "^"

identity :: BOp -> Scalar
identity Add = fromInteger 0

scalarOp :: BOp -> Scalar -> Scalar -> Scalar
scalarOp Add = (+)
scalarOp Mul = (*)

data Expr = Const Scalar | Var String | UExpr UOp Expr | BExpr Expr BOp Expr

parenShow :: Bool -> Expr -> String
parenShow _ (Var v) = v
parenShow _ (Const x) = show x
parenShow _ (UExpr op x) = show op ++ "(" ++ show x ++ ")"

parenShow True x = "(" ++ parenShow False x ++ ")"

parenShow False (BExpr l op r) = 
  parenShow True l ++ " " ++ show op ++ " " ++ parenShow True r

instance Show Expr where
  show = parenShow False

instance Num Expr where
  (+) l r = BExpr l Add r
  (*) l r = BExpr l Mul r
  (-) l r = BExpr l Sub r

  negate x = fromInteger (-1) * x
  abs x = UExpr Abs x
  signum x = UExpr Sign x
  fromInteger x = Const (fromInteger x)

instance Fractional Expr where
  (/) l r = BExpr l Div r
  fromRational x = Const (RaScl x)

exprOp :: BOp -> Expr -> Expr -> Expr
exprOp op l r = BExpr l op r

fromReal :: Double -> Expr
fromReal x = Const (ReScl x)

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
combineConstantsHelper :: BOp -> Expr -> Expr
combineConstantsHelper op (BOp (Const a) op2 (BOp (Const b) op3 c))
  | op == op2 && op2 == op3 = BOp (Const (scalarExpr op a b)) op c'where c' = combineConstantsHelper op c

combineConstantsHelper op (BOp (Const a) op2 (BOp b op3 c))
  | op == op2 && op2 == op3 = BOp (Const a) op c' where c' = combineConstantsHelper op c

combineConstantsHelper op a b = BOp op a b
