module TreeUtils where

import Data.Ratio

import Scalar
import Op
import Expr



touching :: BOp -> Expr -> [Expr]
touching op x = go op [x] []
  where go :: BOp -> [Expr] -> [Expr] -> [Expr]
        go op ((B a op2 b):stack) found
          | op == op2 = go op (b:a:stack) found  -- Reverse order to have the result be in order
        go op (x:stack) found = go op stack (x:found)
        go op [] found = found

neg1 :: Expr
neg1 = exprc (-1)

expandInverse :: Expr -> Expr
expandInverse (B a Sub b) = a + (neg1 * b)
expandInverse (B a Div b) = a + (B b Pow neg1)
expandInverse (U Neg x) = neg1 * x
expandInverse (U Sqrt x) = (B x Pow (fromRational (1 % 2)))
expandInverse (B a op b) = B (expandInverse a) op (expandInverse b)
expandInverse x = x

toSigma :: Expr -> Expr
toSigma (B a Add b) = S (touching Add (B a Add b))
toSigma x = x

toProd :: Expr -> Expr
toProd (B a Mul b) = P (touching Mul (B a Mul b))
toProd x = x
