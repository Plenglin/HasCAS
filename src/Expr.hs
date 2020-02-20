module Expr where

import Debug.Trace
import Scalar
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List

data BOp = Add | Mul | Sub | Div | Pow deriving (Eq, Ord)
data UOp = LApply Expr BOp | RApply BOp Expr | Neg | Sqrt | Abs | Sign | Log | Sin | Cos | Tan | Sec | Csc | Cot deriving (Show, Eq, Ord)

instance Show BOp where
  show Add = "+"
  show Mul = "*"
  show Div = "/"
  show Sub = "-"
  show Pow = "^"

identity :: BOp -> Scalar
identity Add = 0
identity Mul = 1
identity Pow = 1

repeated :: BOp -> BOp
repeated Add = Mul
repeated Mul = Pow

scalarOp :: BOp -> Scalar -> Scalar -> Scalar
scalarOp Add = (+)
scalarOp Mul = (*)
scalarOp Sub = (-)
scalarOp Div = (/)

groupable :: BOp -> Bool
groupable Add = True
groupable Mul = True
groupable _ = False

data Atom = Const Scalar | Var String deriving Eq
instance Show Atom where
  show (Const x) =
    if x < 0
      then "(" ++ show x ++ ")"
      else show x
  show (Var v) = v

instance Ord Atom where
  compare (Const a) (Const b) = compare a b
  compare (Const _) x = LT
  compare _ (Const _) = GT
  compare (Var a) (Var b) = compare a b

-- | A monomial with an empty map represents the value 1.
newtype Monomial = Monomial (Map.Map String Scalar) deriving (Eq, Ord)
instance Show Monomial where
  show (Monomial vs) = concatMap (\(x, p) -> "(" ++ x ++ "^" ++ show p ++ ")") (Map.assocs vs)

mono :: [(String, Scalar)] -> Monomial
mono xs = Monomial (Map.fromAscList xs)

oneMono = Monomial Map.empty

data Expr = A Atom | U UOp Expr | B Expr BOp Expr | I BOp [Expr] | Poly (Map.Map Monomial Scalar) deriving (Eq)
eS = I Add
eP = I Mul

exprv :: String -> Expr
exprv v = A (Var v)

exprc :: Scalar -> Expr
exprc x = A (Const x)

neg1 :: Expr
neg1 = exprc (-1)

poly :: [(Monomial, Scalar)] -> Expr
poly xs = Poly (Map.fromAscList xs)

singlePoly :: Monomial -> Scalar -> Expr
singlePoly m a = poly [(m, a)]

zeroPoly :: Expr
zeroPoly = Poly Map.empty

onePoly :: Expr
onePoly = poly [(oneMono, 1)]

parenShow :: Bool -> Expr -> String
parenShow _ (A a) = show a
parenShow _ (U op x) = show op ++ "(" ++ show x ++ ")"
parenShow False (I Add xs) = "Sigma " ++ show xs
parenShow False (I Mul xs) = "Prod " ++ show xs
parenShow False (I op xs) = show op ++ " " ++ show xs
parenShow False (Poly mons) = "Poly " ++ intercalate " + " (map (\(Monomial m, k) -> show k ++ show (Monomial m)) (Map.assocs mons))

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

instance Ord Expr where
  compare (A a) (A b) = compare a b
  compare (U oa a) (U ob b)
    | oa == ob = compare a b
    | otherwise = compare oa ob

  (<) (A _) _ = True

(^^^) :: Expr -> Expr -> Expr
(^^^) a = B a Pow

exprOp :: BOp -> Expr -> Expr -> Expr
exprOp op l r = B l op r

exprUOp :: UOp -> Expr -> Expr
exprUOp op x = U op x

fromReal :: Double -> Expr
fromReal x = exprc (ReScl x)
