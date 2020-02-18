module Scalar where

import Data.Ratio

data Scalar = IScl Integer | RaScl Rational | ReScl Double

instance Show Scalar where
  show (IScl x) = show x
  show (RaScl x) = show x
  show (ReScl x) = show x

instance Eq Scalar where
  a == b = compare a b == EQ 

instance Ord Scalar where
  compare (IScl a) (IScl b) = compare a b
  compare (IScl a) (RaScl b) = compare (fromInteger a) b
  compare (IScl a) (ReScl b) = compare (fromInteger a) b
  compare (RaScl a) (RaScl b) = compare a b
  compare (RaScl a) (ReScl b) = compare (fromRational a) b
  compare (ReScl a) (ReScl b) = compare a b
  compare a b = compare b a


normalizeTypes :: Scalar -> Scalar -> (Scalar, Scalar)
normalizeTypes (IScl l) (IScl r) = (IScl l, IScl r)
normalizeTypes (IScl l) (RaScl r) = (RaScl (fromInteger l), RaScl r)
normalizeTypes (IScl l) (ReScl r) = (ReScl (fromInteger l), ReScl r)

normalizeTypes (RaScl l) (RaScl r) = (RaScl l, RaScl r)
normalizeTypes (RaScl l) (ReScl r) = (ReScl (fromRational l), ReScl r)
normalizeTypes l r = normalizeTypes r l


instance Num Scalar where
  (+) (IScl l) (IScl r) = IScl (l + r)
  (+) (RaScl l) (RaScl r) = RaScl (l + r)
  (+) (ReScl l) (ReScl r) = ReScl (l + r)
  (+) l r = cl + cr 
    where (cl, cr) = normalizeTypes l r

  (*) (IScl l) (IScl r) = IScl (l * r)
  (*) (RaScl l) (RaScl r) = RaScl (l * r)
  (*) (ReScl l) (ReScl r) = ReScl (l * r)
  (*) l r = cl * cr 
    where (cl, cr) = normalizeTypes l r
  
  (-) (IScl l) (IScl r) = IScl (l - r)
  (-) (RaScl l) (RaScl r) = RaScl (l - r)
  (-) (ReScl l) (ReScl r) = ReScl (l - r)
  (-) l r = cl - cr 
    where (cl, cr) = normalizeTypes l r

  negate (IScl x) = IScl (-x)
  negate (RaScl x) = RaScl (-x)
  negate (ReScl x) = ReScl (-x)

  abs (IScl x) = IScl (abs x)
  abs (RaScl x) = RaScl (abs x)
  abs (ReScl x) = ReScl (abs x)
  
  signum (IScl x) = IScl (signum x)
  signum (RaScl x) = RaScl (signum x)
  signum (ReScl x) = ReScl (signum x)
  
  fromInteger x = IScl (fromInteger x)

instance Fractional Scalar where
  (/) (IScl l) (IScl r) = RaScl (l % r)
  (/) (RaScl l) (RaScl r) = RaScl (l / r)
  (/) (ReScl l) (ReScl r) = ReScl (l / r)
  (/) l r = cl - cr 
    where (cl, cr) = normalizeTypes l r

  fromRational x = RaScl x

