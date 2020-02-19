module Solve where


import qualified Data.Map.Strict as Map

import Expr


data VarPath = None | Var Expr | Split Expr [VarPath]

-- | Finds paths to different variable instances
findVar :: String -> Expr -> [[Expr]]
findVar v x = search [] x
  where search stack (B a op b) =
          let search' = search (B a op b : stack)
            in search' a ++ search' b
        search stack (U op x) = search (U op x : stack) x
        search stack (I op xs) =
          let search' = search (I op xs : stack)
            in concat [search' x | x <- xs]
        search stack (A (Var v2)) 
          | v == v2 = [A (Var v) : stack]
        search stack (Poly xs) = [
            Poly xs : stack | 
            or [Map.member v ms | Monomial ms <- Map.keys xs]
          ]
        search _ _ = []
