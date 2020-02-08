module Main where

import Lib


x = Var "x"
y = Var "y"

a :: Expr
a = fromInteger 5 + (x + (fromInteger 2 * fromInteger 5 * fromInteger 2)) + y + (fromInteger 9 + fromInteger (-3)) + fromInteger 6
eca = evalConstExprs a
sa = straighten Add eca
cca = combineConst Add sa

main :: IO ()
main = do
  putStrLn ("Original: " ++ (show a))
  putStrLn ("Const exprs: " ++ (show eca))
  putStrLn ("Straighten: " ++ (show sa))
  putStrLn ("Reduced: " ++ (show cca))
