{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module HasCAS.Expr where
import Prelude hiding ((+), (*))

import HasCAS.Scalar
class (Show expr, Ring t) => Expr t expr where 
  eval :: Expr t err => expr -> Either t err

data Scalar t where
  Scalar :: Ring t => t -> Scalar t
instance (Ring t) => Show (Scalar t) where
  show (Scalar x) = show x
instance (Ring t) => Expr t (Scalar t) () where 
  eval (Scalar x) = Left x

data Variable = Variable String

data Sum t where 
  Sum :: Expr t => [t] -> Sum t
instance (Ring t) => Show (Sum t) where
  show (Sum x) = "Sum " ++ show x
instance (Ring t) => Expr t (Sum t) where 
  eval (Sum (x:xs)) = eval x + (eval (Sum xs))

data Prod t where 
  Prod :: Expr t => [t] -> Prod t
instance (Ring t) => Show (Prod t) where
  show (Prod x) = "Prod " ++ show x
instance (Ring t) => Expr t (Prod t) where 
  eval (Prod (x:xs)) = x * (eval (Prod xs))
