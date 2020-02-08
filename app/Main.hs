module Main where

import Lib


a :: Expr
a = fromInteger 5 + ((Var "x") + (fromInteger 2 * fromInteger 5 * fromInteger 2))

main :: IO ()
main = do
  putStrLn (show a)
