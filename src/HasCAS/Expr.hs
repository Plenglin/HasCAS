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

type EvalResult t = Either (Expr t) t

fromEvalResult :: EvalResult t -> Expr t
fromEvalResult (Right x) = Scl x
fromEvalResult (Left x) = x

evaluate :: (Ring t, Exponentiable t t t) => Expr t -> EvalResult t
evaluate (Scl x) = Right x
evaluate (Var x) = Left (Var x)

evaluate (Add xs) = 
  let 
    evaluations = map evaluate xs    
    (exprs, scls) = partitionEithers evaluations
    scalarSum = foldr (+) idAdd scls
    exprSum = Add ((Scl scalarSum):exprs)
  in 
    case exprs of
      ([]) -> Right scalarSum
      _ -> Left exprSum

evaluate (Mul xs) = 
  let 
    evaluations = map evaluate xs    
    (exprs, scls) = partitionEithers evaluations
    scalarSum = foldr (*) idMul scls
    exprSum = Add ((Scl scalarSum):exprs)
  in 
    case exprs of
      ([]) -> Right scalarSum
      _ -> Left exprSum

evaluate (Pow b p) =
  let 
    eb = evaluate b
    ep = evaluate p
  in 
    case (eb, ep) of
      (Right sb, Right sp) -> Right (pow sb sp)
      (_, _) -> Left (Pow (fromEvalResult eb) (fromEvalResult ep))
