module Main where

import Lib


x = A (Var "x")
y = A (Var "y")
lhs = ((x + 1) * 2) - (3 * 4)
rhs = exprc 2 - exprc 1
path = findVar "x" lhs
(c, _) = unapplyPath path
sol = applyUOps c rhs 


main :: IO ()
main = do
  print rhs
  print lhs
  print path
  print sol
  print (eval sol)
  
