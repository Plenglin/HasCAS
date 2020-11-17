module HasCAS.Function where
import Prelude hiding ((+), (*), (-), (/), div)


import HasCAS.Scalar
data Terminal a = Open a | Closed a | Inf
data Interval a = Interval (Terminal a) (Terminal a)

data Function a = Function 
  { apply :: a -> a
  , name :: String
  , inverse :: Maybe (Function a)
  }

instance Show (Function a) where 
  show f = name f

instance Eq (Function a) where 
  f == g = (name f) == (name g)

-- | Add a value.
add :: (Ring t) => t -> Function t
add k = Function
  { name = "(+" ++ show k ++ ")"
  , apply = \x -> x + k
  , inverse = Just (sub k)  
  }

-- | Subtract a value.
sub :: (Ring t) => t -> Function t
sub k = Function 
  { name = "(-" ++ show k ++ ")"
  , apply = \x -> x - k
  , inverse = Just (add k)
  }

-- | Scale by a value.
mul :: (DivRing t) => t -> Function t
mul k = Function
  { name = "(*" ++ show k ++ ")"
  , apply = \x -> x * k
  , inverse = Just (div k)
  }

-- | Divide by a value.
div :: (DivRing t) => t -> Function t
div k = Function
  { name = "(/" ++ show k ++ ")"
  , apply = \x -> x / k
  , inverse = Just (mul k)
  }

-- | Reciprocal function
reciprocal :: (DivRing t) => Function t
reciprocal = Function
  { name = "(1/)"
  , apply = \x -> inv x
  , inverse = Just reciprocal
  }

-- | Multiply by a value on a ring without division.
mulRing :: (Ring t) => t -> Function t
mulRing k = Function 
  { name = "(*" ++ show k ++ ")"
  , apply = \x -> x * k
  , inverse = Nothing
  }

-- | Iterated multiplication by itself.
partialPow :: (DivRing exp, Exponentiable a exp a) => exp -> Function a
partialPow p = Function
  { name = "(^" ++ show p ++ ")"
  , apply = \x -> pow x p
  , inverse = Just (partialPow (inv p))
  }
