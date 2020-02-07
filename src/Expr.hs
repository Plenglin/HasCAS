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

reducer :: BOp -> Scalar -> Scalar -> Scalar
reducer Add = (+)

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

combine :: BOp -> Expr -> Expr -> Expr
combine op l r = BExpr l op r

fromReal :: Double -> Expr
fromReal x = Const (ReScl x)

rotateLeft :: Expr -> Expr
rotateLeft (BExpr (BExpr a o1 b) o2 c) = BExpr a o1 (BExpr b o2 c)

rotateRight :: Expr -> Expr
rotateRight (BExpr a o1 (BExpr b o2 c)) = BExpr (BExpr a o1 b) o2 c

{-- | Returns the expression that should be there
processExpr :: BOp -> Expr-> Expr
processExpr (BExp)


processExpr op acc vars expr = (acc, expr:vars)


combineLikeTerms :: BOp -> Expr -> Expr
combineLikeTerms op ex = foldl (combine op) (Const const) vars
  where (const, vars) = processExpr op (identity op) [] ex
-}