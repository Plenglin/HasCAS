{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module HasCAS.Expr where
import Prelude hiding ((+), (*))

import HasCAS.Scalar


data Expr a = Add [a] | Mul [a] | Pow (Expr a) a | F 