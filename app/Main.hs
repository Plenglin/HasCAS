module Main where

import Lib


ex :: Expr
ex = Const (fromInteger 5) + Var "x"

main :: IO ()
main = putStrLn (show ex)
