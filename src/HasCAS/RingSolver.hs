module HasCAS.RingSolver where 


import HasCAS.Scalar
import HasCAS.Expr
import HasCAS.Equation
import HasCAS.Function
import Data.Either ( partitionEithers )

data SplitReducer = Sum | Prod
data VarTree a = Term String | Split SplitReducer [VarTree a] | Apply (Function (Expr a)) (VarTree a)



containsSplits :: VarTree a -> Bool 
containsSplits (Term _) = False
containsSplits (Split _ _) = True 
containsSplits (Apply _ t) = containsSplits t

findVariables :: Ring a => Expr a -> Either (Expr a) (VarTree a)
findVariables (Var v) = Right (Term v)
findVariables c@(Scl _) = Left c
findVariables (Add l) = iterFindVariables l (\a b -> Add a b) 

iterFindVariables xs reducer f exType = 
  let 
    searchResults = map findVariables xs
    (consts, vars) = partitionEithers searchResults
    const = exType consts
    joinVar l@(_:_:_) = Split reducer l
    joinVar [x] = x
  in 
    if null vars
      then Left (exType xs)
      else Right (Apply (f const) (joinVar vars))



--decomposePath :: Ring a => Expr a -> [Function (Expr a)]


solveSV :: DivRing a => Equation a -> Expr a
solveSV x = undefined