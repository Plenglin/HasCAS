module HasCAS.Equation where

import HasCAS.Expr
-- | An equation implicitly set equal to 0.
data Equation a = Equation (Expr a) Ordering

invOrdering :: Ordering -> Ordering
invOrdering EQ = EQ
invOrdering LT = GT 
invOrdering GT = LT
