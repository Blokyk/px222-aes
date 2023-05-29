{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Algebra.Polynomial (
      Polynomial()
    , coeffs
    , polynomial
    , mapPolynomial
    , degree
    , divEuclide
    , polyMod
    , polyDiv
    , multScalaire
    , applyPolynomial
    , computePolynomialAt
    , modInv
    , polyGCDExt
) where

import Algebra.Ring
import Algebra.Field

import Utils (zipWithDefault, withIndex)
import Data.List (intercalate)
import Control.Exception (assert, throw, ArithException(DivideByZero))
import Data.Foldable (Foldable(foldl'))

-- note: we don't want to export the ctor directly since consumers/users
-- could create a non-trimmed polynomial, which would break quite a few
-- assumptions in our code
newtype Polynomial a = Polynomial [a] deriving (Eq)

-- | Returns the coefficients of the polynomial, by increasing degrees.
-- (e.g., for @2*X^2 + 1*X + 3@, this would give @[3, 1, 2]@)
--
-- The following property always holds:
--
-- > polynomial . coeffs = id
--
-- Assuming the list given to 'polynomial' contains no trailing zeroes,
-- we also get:
--
-- > coeffs . polynomial = id
coeffs :: Polynomial a -> [a]
coeffs (Polynomial a) = a

-- | Creates a polynomial with the given coefficients, treating the first
-- element as the coefficient with the lowest degree. For example:
--
-- >>> polynomial [2, 8, 0, 3]
-- 3*X^3 + 8*X + 2
polynomial :: Field a => [a] -> Polynomial a
polynomial = trim . Polynomial

-- | The degree of the polynomial, or -1 in case it's the null polynomial
degree :: Polynomial a -> Int
-- Ideally, `degree (Polynomial [])` should be undefined (technically Infinity, but it doesn't exist for ints),
-- but defining it as -1 makes it easier for us in comparisons etc
degree (Polynomial a)  = length a - 1

nullPolynomial :: Polynomial a
nullPolynomial = Polynomial []

-- | Raises the degree of a polynomial by 1
raiseDegree :: Field a => Polynomial a -> Polynomial a
raiseDegree (Polynomial []) = Polynomial []
raiseDegree (Polynomial a)  = Polynomial (zero : a)

-- | Increases the degree of the polynomial to the given value
padUntilDegree :: Field a => Int -> Polynomial a -> Polynomial a
padUntilDegree n p
    | degree p >= n = p
    -- we want to make sure we're not trapped in an infinite loop
    -- in case this is actually the null/zero-polynomial
    | otherwise     = assert (p /= zero) $ padUntilDegree n (raiseDegree p)


--divEuclid takes two Polynoms (dividend and divisor) , returns a couple of Polynoms (quotient, rest)
--and treats 3 cases : if the divisor is zero, if the dividend's degree is greater than the divisor's one and so the quotient is null and the rest is trimmed;
-- the third case is the operation : the quotient is composed by the sum 
infixl 7 `divEuclide`
divEuclide :: Field a => Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
divEuclide dividend@(Polynomial a) divisor@(Polynomial b)
    | divisor == zero                  = throw DivideByZero
    | degree dividend < degree divisor = (nullPolynomial, trim dividend)
    | otherwise
        = (Polynomial (subFactors ++ replicate zeroFactorCount zero ++ [factor]), reste)
            where
                factor                         = diviser (last a) (last b)
                subRes = dividend `sub` multScalaire factor (padUntilDegree (degree dividend) divisor)
                (Polynomial subFactors, reste) = divEuclide subRes divisor
                zeroCount = degree dividend - degree subRes - 1
                zeroFactorCount =
                    if zeroCount == 0
                        then 0
                        else min zeroCount (degree dividend - degree divisor)

infixl 7 `polyMod`
-- | Returns the first polynomial modulo the second
polyMod :: Field a => Polynomial a -> Polynomial a -> Polynomial a
polyMod a b = snd $ divEuclide a b

infixl 7 `polyDiv`
polyDiv :: Field a => Polynomial a -> Polynomial a -> Polynomial a
polyDiv a b = fst $ divEuclide a b

addPolynomial :: Field a => Polynomial a -> Polynomial a -> Polynomial a
addPolynomial (Polynomial []) q = q
addPolynomial p (Polynomial []) = p
addPolynomial (Polynomial a) (Polynomial b) = polynomial (zipWithDefault add zero zero a b)

infix 7 `multScalaire`
-- | Multiplies a polynomial by a scalar, i.e. a value from the coefficient's field
multScalaire :: Field a => a -> Polynomial a -> Polynomial a
multScalaire lambda (Polynomial a)
    | lambda == zero = nullPolynomial
    | otherwise      = Polynomial (map (mult lambda) a)

-- This computes the result by multiplying the second polynomial (q) by each coefficient, increasing
-- the degree each time, i.e.:
--      p*q = a0*q + X*(a1*q) + X²*(a2*q) + ...
-- It does this by recursion, first calculating `a0*q`, then 'making' a new polynomial with the other
-- coefficients, and multiplying that by q, and finally raising the degree and adding
-- it to `a0*q`:
--          = a0*q + X*(a1*q + X*(a2*q + ...))
--                      ~~~~~~~~~~~~~~~~~~~~~
--                     = (a1 + a2*X + ...) * q
multPolynomial :: Field a => Polynomial a -> Polynomial a -> Polynomial a
multPolynomial (Polynomial []) _ = nullPolynomial
multPolynomial (Polynomial (x:xs)) q
    = add
        (multScalaire x q) -- a0*q
        (raiseDegree $ multPolynomial (polynomial xs) q) -- X*(a1*q + X*(a2*q + ...))

infixl 7 `modInv`
modInv :: Field a => Polynomial a -> Polynomial a -> Polynomial a
modInv b m = a
  where
    (_, (a, _)) = polyGCDExt b m

polyGCDExt :: Field a => Polynomial a -> Polynomial a -> (Polynomial a, (Polynomial a, Polynomial a))
polyGCDExt p q
    | q == zero = (p, (one, zero))
    | otherwise = (g, (a, c))
    where
        (g, (prev_a, prev_c)) = polyGCDExt q (p `polyMod` q)
        a  = prev_c
        c  = prev_a `sub` (prev_c `mult` (p `polyDiv` q))

-- | Removes all highest-degree coefficients that are zero
trim :: Ring a => Polynomial a -> Polynomial a
trim p = Polynomial $ trim' $ coeffs p
    where
        trim' [] = []
        trim' (x:xs)
            | x == zero && null trimmedXS = []
            | otherwise = x:trimmedXS
            where trimmedXS = trim' xs

mapPolynomial :: (Field b) => (a -> b) -> Polynomial a -> Polynomial b
mapPolynomial f = polynomial . map f . coeffs

infixl 9 `computePolynomialAt`
computePolynomialAt :: Ring a => Polynomial a -> a -> a
computePolynomialAt (Polynomial as) x
    = foldl' add zero valueByDegrees
    where
        valueByDegrees = map (\(a, pow) -> if a == zero then zero else a `mult` (powers !! pow)) $ withIndex as
        powers = iterate (mult x) one

applyPolynomial :: Ring b => (a -> b -> b) -> Polynomial a -> b -> b
applyPolynomial f (Polynomial as) x
    = foldl' add zero valueByDegrees
    where
        valueByDegrees = map (\(a, pow) -> a `f` (powers !! pow)) $ withIndex as
        powers = iterate (mult x) one

instance (Field a) => Ring (Polynomial a) where
    -- takes each coeff and adds them together; in case there's more coeffs on one side,
    -- it will just use `zero` to 'pad' the shorter one with zeroes
    zero = Polynomial []
    add = addPolynomial
    add_inverse (Polynomial a) = Polynomial (map add_inverse a)
    mult = multPolynomial
    one = Polynomial [one]

instance (Show a, Ring a) => Show (Polynomial a) where
    show (Polynomial []) = show (zero :: a)
    show (Polynomial a)
        | a == [zero] = show a
        | otherwise
        = intercalate " + " -- put a '+' between each expression
        $ filter ("" /=)   -- ignore empty strings
        $ map formatCoeff $ withIndex a
        where formatCoeff (coeff, deg)
                | coeff == zero = ""
                | deg == 0      = show coeff
                | coeff == one  = "X^" ++ show deg
                | otherwise     = show coeff ++ "*X^" ++ show deg
