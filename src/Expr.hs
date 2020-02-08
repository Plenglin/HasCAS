module Expr where

import Debug.Trace
import Scalar

data BOp = Add | Mul | Sub | Div | Pow deriving Eq
data UOp = Abs | Sign deriving (Show, Eq)

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
scalarOp Sub = (-)
scalarOp Div = (/)

data Expr = Const Scalar | Var String | UExpr UOp Expr | BExpr Expr BOp Expr deriving Eq

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
