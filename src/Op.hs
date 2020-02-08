module Op where


import Scalar

data BOp = Add | Mul | Sub | Div | Pow deriving Eq
data UOp = Abs | Sign | Log | Sin | Cos | Tan | Sec | Csc | Cot deriving (Show, Eq)

instance Show BOp where
  show Add = "+"
  show Mul = "*"
  show Div = "/"
  show Sub = "-"
  show Pow = "^"

identity :: BOp -> Scalar
identity Add = fromInteger 0
identity Mul = fromInteger 1

scalarOp :: BOp -> Scalar -> Scalar -> Scalar
scalarOp Add = (+)
scalarOp Mul = (*)
scalarOp Sub = (-)
scalarOp Div = (/)

