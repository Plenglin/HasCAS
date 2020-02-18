module Main where

import Lib


x = A (Var "x")
y = A (Var "y")
e = ((x + (x + 1) ^^^ 2) ^^^ 2)
expanded = expandPolynomial e

main :: IO ()
main = do
  putStrLn (show (length xs))
  putStrLn (show expanded)
  where (Poly xs) = expanded
