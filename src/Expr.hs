module Expr where

import Scalar

data BOp = Add | Mul | Sub | Div | Pow deriving Show
data UOp = Abs | Sign deriving Show

data Expr = Const Scalar | Var String | UExpr UOp Expr | BExpr Expr BOp Expr deriving Show

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

fromReal :: Double -> Expr
fromReal x = Const (ReScl x)
