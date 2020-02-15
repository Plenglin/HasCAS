module Op where


import Scalar

data BOp = Add | Mul | Sub | Div | Pow deriving (Eq, Ord)
data UOp = Neg | Sqrt | Abs | Sign | Log | Sin | Cos | Tan | Sec | Csc | Cot deriving (Show, Eq, Ord)

instance Show BOp where
  show Add = "+"
  show Mul = "*"
  show Div = "/"
  show Sub = "-"
  show Pow = "^"

identity :: BOp -> Scalar
identity Add = 0
identity Mul = 1

repeated :: BOp -> BOp
repeated Add = Mul
repeated Mul = Pow

scalarOp :: BOp -> Scalar -> Scalar -> Scalar
scalarOp Add = (+)
scalarOp Mul = (*)
scalarOp Sub = (-)
scalarOp Div = (/)

