{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use <$>" #-}

module Algebra.Polynomial where

import Algebra.Ring
import Algebra.Field

import Utils (zipWithDefault)
import Data.List (intercalate)

-- note: we don't want to export the ctor directly since consumers/users
-- could create a non-trimmed polynomial, which would break quite a few
-- assumptions in our code
newtype Polynomial a = Polynomial [a] deriving (Eq)

instance (Show a, Field a) => Show (Polynomial a) where
    show (Polynomial a)
        = intercalate " + " -- put a '+' between each expression
        $ filter ("" /= )   -- ignore empty strings
        $ zipWith formatCoeff a ([0..] :: [Int])
        where formatCoeff coeff deg
                | coeff == zero = ""
                | deg == 0      = show coeff
                | coeff == one  = "X^" ++ show deg
                | otherwise     = show coeff ++ "*X^" ++ show deg

coeffs :: Polynomial a -> [a]
coeffs (Polynomial a) = a

polynomial :: Field a => [a] -> Polynomial a
polynomial = trim . Polynomial

degree :: Polynomial a -> Int
-- Ideally, `degree (Polynomial [])` should be undefined (technically Infinity, but it doesn't exist for ints),
-- but defining it as -1 makes it easier for us in comparisons etc
degree (Polynomial a)  = length a - 1

nullPolynomial :: Polynomial a
nullPolynomial = Polynomial []

raiseDegree :: Field a => Polynomial a -> Polynomial a
raiseDegree (Polynomial a) = Polynomial (zero : a)

padUntilDegree :: Field a => Int -> Polynomial a -> Polynomial a
padUntilDegree n p
    | degree p >= n = p
    | otherwise     = padUntilDegree n (raiseDegree p)

divEuclide :: Field a => Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
divEuclide dividend@(Polynomial a) divisor@(Polynomial b)
    | degree divisor <= 0              = undefined
    | degree dividend < degree divisor = (nullPolynomial, dividend)
    | otherwise
        = (Polynomial (subFactors ++ [factor]), reste)
            where factor                         = diviser (last a) (last b)
                  (Polynomial subFactors, reste) = divEuclide (trim $ sub dividend (multScalaire factor (padUntilDegree (degree dividend) divisor))) divisor

polyMod :: Field a => Polynomial a -> Polynomial a -> Polynomial a
polyMod a b = snd $ divEuclide a b

addPolynomial :: Field a => Polynomial a -> Polynomial a -> Polynomial a
addPolynomial (Polynomial a) (Polynomial b) = polynomial (zipWithDefault add zero zero a b)

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

trim :: Field a => Polynomial a -> Polynomial a
trim p@(Polynomial []) = p
trim p@(Polynomial a)
    | last a /= zero = p
    | otherwise      = trim (Polynomial $ init a)

instance (Field a) => Ring (Polynomial a) where
    -- takes each coeff and adds them together; in case there's more coeffs on one side,
    -- it will just use `zero` to 'pad' the shorter one with zeroes
    zero = Polynomial []
    add = addPolynomial
    add_inverse (Polynomial a) = Polynomial (map add_inverse a)
    mult = multPolynomial
    one = Polynomial [one]