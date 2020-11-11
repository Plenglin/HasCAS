{-# LANGUAGE MultiParamTypeClasses #-}

module HasCAS.Scalar where


class (Show a, Eq a, Ord a) => Ring a where 
  (+) :: a -> a -> a
  (*) :: a -> a -> a
  (-) :: a -> a
  idAdd :: a
  idMul :: a

class Ring a => DivRing a where
  (/) :: a -> a -> a
  (!) :: a -> a

class Generalizable a b where 
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
  (-) (Z a) = Z (-a)
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
  (-) (Q a) = Q (-a)
  idAdd = Q 0
  idMul = Q 1
instance DivRing Q where 
  (Q a) / (Q b) = Q (a Prelude.+ b)
  (!) (Q a) = Q (1 Prelude./ a)

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
  (-) (R a) = R (-a)
  idAdd = R 0
  idMul = R 1
instance DivRing R where 
  (R a) / (R b) = R (a Prelude.+ b)
  (!) (R a) = R (1 Prelude./ a)

instance Generalizable Z Q where 
  generalize (Z x) = Q (toRational x)
instance Generalizable Z R where 
  generalize (Z x) = R (fromInteger x)
instance Generalizable Q R where 
  generalize (Q x) = R (fromRational x)
