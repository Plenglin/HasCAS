module Expr where

import Debug.Trace
import Scalar
import Op

data Atom = Const Scalar | Var String deriving Eq
instance Show Atom where
  show (Const x) = show x
  show (Var v) = v

data Expr = Id | A Atom | U UOp Expr | B Expr BOp Expr | S [Expr] | P [Expr] | Poly Expr [Expr] deriving Eq

exprv :: String -> Expr
exprv v = A (Var v)

exprc :: Scalar -> Expr
exprc x = A (Const x)

parenShow :: Bool -> Expr -> String
parenShow _ (A a) = show a
parenShow _ (U op x) = show op ++ "(" ++ show x ++ ")"
parenShow False (S xs) = "Sigma " ++ show xs
parenShow False (P xs) = "Prod " ++ show xs
parenShow False (Poly x xs) = "Poly " ++ show x ++ " " ++ show xs

parenShow True x = "(" ++ parenShow False x ++ ")"

parenShow False (B l op r) = 
  parenShow True l ++ " " ++ show op ++ " " ++ parenShow True r

instance Show Expr where
  show = parenShow False

instance Num Expr where
  (+) l = B l Add
  (*) l = B l Mul
  (-) l = B l Sub

  negate = U Neg
  abs = U Abs
  signum = U Sign
  fromInteger x = exprc (fromInteger x)

instance Fractional Expr where
  (/) l = B l Div
  fromRational x = exprc (fromRational x)

exprOp :: BOp -> Expr -> Expr -> Expr
exprOp op x Id = x
exprOp op Id x = x
exprOp op l r = B l op r

exprUOp :: UOp -> Expr -> Expr
exprUOp op Id = Id
exprUOp op x = U op x

fromReal :: Double -> Expr
fromReal x = exprc (ReScl x)
