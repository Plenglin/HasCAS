{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module HasCAS.Expr where
import Prelude hiding ((+), (*))

import HasCAS.Scalar
import HasCAS.Function
import Data.Either


data Expr a = 
  Scl a
  | Var String  
  | Add [Expr a] 
  | Mul [Expr a] 
  | Pow (Expr a) (Expr a)
  | F (Function a) (Expr a)
  deriving (Show, Eq)

instance Ring a => Ring (Expr a) where
  a + b = Add [a, b]
  a * b = Mul [a, b]
  idAdd = Scl idAdd
  idMul = Scl idMul


type EvalResult t = Either (Expr t) t

fromEvalResult :: EvalResult t -> Expr t
fromEvalResult (Right x) = Scl x
fromEvalResult (Left x) = x

evaluate :: (Ring t) => Expr t -> EvalResult t
evaluate (Scl x) = Right x
evaluate (Var x) = Left (Var x)

evaluate (Add xs) = iterOp (+) idAdd Add xs

evaluate (Mul xs) = iterOp (*) idMul Mul xs

iterOp op i agg xs = 
  let 
    evaluations = map evaluate xs    
    (exprs, scls) = partitionEithers evaluations
    scalarSum = foldr op i scls
    exprSum = agg ((Scl scalarSum):exprs)
  in 
    case exprs of
      ([]) -> Right scalarSum
      _ -> Left exprSum
