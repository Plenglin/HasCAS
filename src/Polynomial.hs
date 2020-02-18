module Polynomial where

import Data.Bits
import qualified Data.Map as Map
    
import Expr
import Op
import Scalar
import TreeUtils

-- | Removes zero powers from the monomial.
pruneMonomial :: Monomial -> Monomial
pruneMonomial (Monomial m) = Monomial (Map.filter (/=0) m)

-- | Removes zero terms from the polynomial
prunePolynomial :: Expr -> Expr
prunePolynomial (Poly xs) = Poly (Map.filter (/=0) xs)

-- | Assumes that the given polynomials have been simplified.
addPolynomials :: Expr -> Expr -> Expr
addPolynomials (Poly ams) (Poly bms) = Poly unioned
  where unioned = Map.unionWith (+) ams bms

-- | Scales a polynomial by a scalar.
sclPolynomial :: Scalar -> Expr -> Expr
sclPolynomial a (Poly xs) 
  | a == 0 = zeroPoly
  | otherwise = Poly (Map.map (*a) xs)

-- | Returns the product of two monomials, pruned if necessary.
mulMonomials :: Monomial -> Monomial -> Monomial
mulMonomials (Monomial m1) (Monomial m2) = pruneMonomial (Monomial unioned)
  where unioned = Map.unionWith (+) m1 m2

-- | Returns the product of a monomial and a polynomial.
mulMonomialPolynomial :: Monomial -> Expr -> Expr
mulMonomialPolynomial (Monomial m) (Poly xs)  
  | null m = Poly xs
  | otherwise = prunePolynomial (Poly (Map.fromAscList terms'))
      where terms = Map.assocs xs
            terms' = [(mulMonomials (Monomial m) m2, p) | (m2, p) <- terms]

-- | Returns the product of two polynomials, assuming that the given polynomials have been simplified.
mulPolynomials :: Expr -> Expr -> Expr
mulPolynomials (Poly ams) (Poly bms) = prunePolynomial sum
  where products = [sclPolynomial aa (mulMonomialPolynomial am (Poly bms)) | (am, aa) <- Map.assocs ams]
        sum = foldl addPolynomials zeroPoly products

powPolynomial :: Scalar -> Expr -> Expr
powPolynomial (IScl 0) _ = poly [(oneMono, 1)]
powPolynomial (IScl 1) x = x
powPolynomial (IScl i) x
  | i > 1 = esq x onePoly i
    where esq sq acc 0 = acc
          esq sq acc 1 = mulPolynomials sq acc
          esq sq acc n = let 
              sq' = mulPolynomials sq sq
              half = n `shift` (-1)
              shouldPow = odd n
            in esq sq' (if shouldPow then mulPolynomials sq acc else acc) half

-- | Given a pure polynomial expression, expands it out to a single polynomial.
expandPolynomial :: Expr -> Expr
expandPolynomial (B a Add b) = expandPolynomial (liftAssociative (B a Add b))
expandPolynomial (B a Mul b) = expandPolynomial (liftAssociative (B a Mul b))
expandPolynomial (B a Pow (A (Const (IScl p)))) = powPolynomial (IScl p) (expandPolynomial a) 
expandPolynomial (I Add xs) = prunePolynomial (foldl addPolynomials zeroPoly polys)
  where polys = map expandPolynomial xs
expandPolynomial (I Mul xs) = prunePolynomial (foldl mulPolynomials onePoly polys)
  where polys = map expandPolynomial xs
expandPolynomial (Poly xs) = Poly xs
expandPolynomial (A (Var v)) = poly [(mono [(v, 1)], 1)]
expandPolynomial (A (Const x)) = poly [(oneMono, x)]
