module Algebra.Polynomial (
      Polynomial()
    , coeffs
    , polynomial
    , mapPolynomial
    , degree
    , divEuclide
    , polyMod
    , multScalaire
    , applyPolynomial
) where

import Algebra.Ring
import Algebra.Field

import Utils (zipWithDefault, withIndex)
import Data.List (intercalate)
import Control.Exception (assert, throw, ArithException(DivideByZero))

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
raiseDegree (Polynomial a) = Polynomial (zero : a)

-- | Increases the degree of the polynomial to the given value
padUntilDegree :: Field a => Int -> Polynomial a -> Polynomial a
padUntilDegree n p
    | degree p >= n = p
    -- we want to make sure we're not trapped in an infinite loop
    -- in case this is actually the null/zero-polynomial
    | otherwise     = assert (p /= zero) $ padUntilDegree n (raiseDegree p)

-- | Returns the quotient and remainder of the euclidean division of two polynomials
divEuclide :: Field a => Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
divEuclide dividend@(Polynomial a) divisor@(Polynomial b)
    | degree divisor  < 0              = throw DivideByZero
    | degree dividend < degree divisor = (nullPolynomial, dividend)
    | otherwise
        = (Polynomial (subFactors ++ [factor]), reste)
            where factor                         = diviser (last a) (last b)
                  (Polynomial subFactors, reste) = divEuclide (trim $ sub dividend (multScalaire factor (padUntilDegree (degree dividend) divisor))) divisor

-- | Returns the the first polynomial modulo the second
polyMod :: Field a => Polynomial a -> Polynomial a -> Polynomial a
polyMod a b = snd $ divEuclide a b

addPolynomial :: Field a => Polynomial a -> Polynomial a -> Polynomial a
addPolynomial (Polynomial a) (Polynomial b) = polynomial (zipWithDefault add zero zero a b)

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

applyPolynomial :: Ring b => (a -> b -> b) -> Polynomial a -> b -> b
applyPolynomial f (Polynomial as) x
    = foldr add zero valueByDegrees
    where valueByDegrees = map (\(a, pow) -> a `f` (iterate (mult x) one !! pow)) $ withIndex as

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
