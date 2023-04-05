{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use <$>" #-}

module Algebra.Polynomial where

import Algebra.Ring
import Algebra.Field

import Utils (zipWithDefault)

newtype Polynomial a = Polynomial [a] deriving (Eq,Show)

degree :: Polynomial a -> Int
degree (Polynomial []) = -1 -- should be undefined, but this makes it easier for us
degree (Polynomial a)  = length a - 1

convolve :: Field a => [a] -> [a] -> [a]
convolve xs ys
    = do
        x <- xs
        y <- ys
        return (mult x y)

nullPolynomial :: Polynomial a
nullPolynomial = Polynomial []

padUntilDegree :: Field a => Int -> Polynomial a -> Polynomial a
padUntilDegree n p
    | degree p >= n = p
    | otherwise = padUntilDegree n (mult p (Polynomial [zero, one]))

divEuclide :: Field a => Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
divEuclide dividend@(Polynomial a) divisor@(Polynomial b)
    | degree divisor <= 0              = undefined
    | degree dividend < degree divisor = (nullPolynomial, dividend)
    | otherwise
        = (Polynomial (subFactors ++ [factor]), reste)
            where factor                         = diviser (last a) (last b)
                  (Polynomial subFactors, reste) = divEuclide (sub dividend (mult_scalaire factor (padUntilDegree (degree dividend) divisor))) divisor

mod_polynomial :: Field a => Polynomial a -> Polynomial a -> Polynomial a
mod_polynomial a b = snd $ divEuclide a b

reduce :: Field a => Polynomial a -> Polynomial a
reduce = mod_polynomial irreducible_m

irreducible_m :: Field a => Polynomial a
--                          x^0  x^1        x^3  x^4                    x^8
irreducible_m = Polynomial [one, one, zero, one, one, zero, zero, zero, one]

add_polynomial :: Field a => Polynomial a -> Polynomial a -> Polynomial a
add_polynomial (Polynomial a) (Polynomial b) = Polynomial (zipWithDefault add zero zero a b)

mult_scalaire :: Field a => a -> Polynomial a -> Polynomial a
mult_scalaire a = mult (Polynomial [a])

instance (Field a) => Ring (Polynomial a) where
    -- takes each coeff and adds them together; in case there's more coeffs on one side,
    -- it will just use `zero` to 'pad' the shorter one with zeroes
    add = add_polynomial
    zero = Polynomial []
    add_inverse (Polynomial a) = Polynomial (map add_inverse a)
    mult (Polynomial a) (Polynomial b) = error "mult doesn't work!!!"
    one = Polynomial [one]