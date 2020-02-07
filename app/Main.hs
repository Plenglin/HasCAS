module Main where

import Lib


ex :: Expr
ex = fromInteger 5 + Var "x" + (Var "y" * Var "z") + fromInteger 8 + fromInteger 2

main :: IO ()
main = putStrLn "T"--(show (combineLikeTerms Add ex))
