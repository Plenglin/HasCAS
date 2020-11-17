{-# LANGUAGE MultiParamTypeClasses #-}

module HasCAS.Scalar where
import qualified Prelude
import Prelude hiding ((+), (*), (-), (/), (^))

class (Show a, Eq a, Ord a) => Ring a where 
  -- | Addition
  (+) :: a -> a -> a
  -- | Multiplication
  (*) :: a -> a -> a
  -- | Negation
  neg :: a -> a
  idAdd :: a
  idMul :: a

(-) :: (Ring t) => t -> t -> t
a - b = a + (neg b)

class Ring a => DivRing a where
  inv :: a -> a

(/) :: (DivRing t) => t -> t -> t
a / b = a * (inv b)

class Exponentiable a b c where 
  pow :: a -> b -> c

class Generalizable b a where 
  generalize :: a -> b

data Z = Z Integer
instance Eq Z where 
  (Z a) == (Z b) = a == b
instance Ord Z where 
  compare (Z a) (Z b) = compare a b
instance Show Z where 
  show (Z x) = show x
instance Ring Z where
  (Z a) + (Z b) = Z (a Prelude.+ b)
  (Z a) * (Z b) = Z (a Prelude.* b)
  neg (Z a) = Z (-a)
  idAdd = Z 0
  idMul = Z 1

data Q = Q Rational
instance Eq Q where 
  (Q a) == (Q b) = a == b
instance Ord Q where 
  compare (Q a) (Q b) = compare a b
instance Show Q where 
  show (Q x) = show x
instance Ring Q where 
  (Q a) + (Q b) = Q (a Prelude.+ b)
  (Q a) * (Q b) = Q (a Prelude.* b)
  neg (Q a) = Q (-a)
  idAdd = Q 0
  idMul = Q 1
instance DivRing Q where 
  inv (Q a) = Q (1 Prelude./ a)

data R = R Double
instance Eq R where 
  (R a) == (R b) = a == b
instance Ord R where 
  compare (R a) (R b) = compare a b
instance Show R where 
  show (R x) = show x
instance Ring R where 
  (R a) + (R b) = R (a Prelude.+ b)
  (R a) * (R b) = R (a Prelude.* b)
  neg (R a) = R (-a)
  idAdd = R 0
  idMul = R 1
instance DivRing R where 
  inv (R a) = R (1 Prelude./ a)

instance Generalizable Q Z where 
  generalize (Z x) = Q (toRational x)
instance Generalizable R Z where 
  generalize (Z x) = R (fromInteger x)
instance Generalizable R Q where 
  generalize (Q x) = R (fromRational x)
