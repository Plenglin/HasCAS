module Polynomial where

import qualified Data.Map as Map
    
import Expr
import Op
import Scalar
import TreeUtils

-- | Removes zero powers from the monomial.
pruneMonomial :: Monomial -> Monomial
pruneMonomial (Monomial m) = Monomial (Map.filter (==0) m)

-- | Moves 1-Monomials into the constant term.

-- | Assumes that the given polynomials have been simplified.
addPolynomials :: Expr -> Expr -> Expr
addPolynomials (Poly ams) (Poly bms) = Poly unioned
  where unioned = Map.unionWith (+) ams bms

sclPolynomial :: Scalar -> Expr -> Expr
sclPolynomial a (Poly xs) = Poly (Map.map (*a) xs)

mulMonomials :: Monomial -> Monomial -> Monomial
mulMonomials (Monomial m1) (Monomial m2) = Monomial unioned
  where unioned = Map.unionWith (+) m1 m2

mulMonomialPolynomial :: Monomial -> Expr -> Expr
mulMonomialPolynomial (Monomial m) (Poly xs)  
  | null m = Poly xs
  | otherwise = Poly (Map.fromAscList terms')
      where terms = Map.assocs xs
            terms' = [(mulMonomials (Monomial m) m2, p) | (m2, p) <- terms]

-- | Assumes that the given polynomials have been simplified.
mulPolynomials :: Expr -> Expr -> Expr
mulPolynomials (Poly ams) (Poly bms) = sum
  where products = [sclPolynomial aa (mulMonomialPolynomial am (Poly bms)) | (am, aa) <- Map.assocs ams]
        sum = foldl addPolynomials zeroPoly products

expandPolynomial :: Expr -> Expr
expandPolynomial (B a Add b) = expandPolynomial (liftAssociative (B a Add b))
expandPolynomial (B a Mul b) = expandPolynomial (liftAssociative (B a Mul b))
expandPolynomial (I Add xs) = foldl addPolynomials zeroPoly polys
  where polys = map expandPolynomial xs
expandPolynomial (I Mul xs) = foldl mulPolynomials zeroPoly polys
  where polys = map expandPolynomial xs