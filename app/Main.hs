module Main where

import Lib


x = A (Var "x")
y = A (Var "y")
e = ((x + (y ^^^ 2) + 1) ^^^ 10)
expanded = expandPolynomial e

main :: IO ()
main = do
  putStrLn (show (length xs))
  putStrLn (show expanded)
  where (Poly xs) = expanded
