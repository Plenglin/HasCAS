module Expr where

import Debug.Trace
import Scalar
import Op


data Expr = Id | Const Scalar | Var String | UExpr UOp Expr | BExpr Expr BOp Expr deriving Eq

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
exprOp op x Id = x
exprOp op Id x = x
exprOp op l r = BExpr l op r

exprUOp :: UOp -> Expr -> Expr
exprUOp op Id = Id
exprUOp op x = UExpr op x

fromReal :: Double -> Expr
fromReal x = Const (ReScl x)
