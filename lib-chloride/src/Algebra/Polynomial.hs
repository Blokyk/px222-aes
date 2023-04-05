{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Algebra.Polynomial where

import Algebra.Ring
import Algebra.Field

import Utils (zipWithDefault)

newtype Polynomial a = Polynomial [a] deriving (Eq,Show)

degree :: Polynomial a -> Int
degree (Polynomial []) = undefined
degree (Polynomial a)  = length a - 1

convolve :: Field a => [a] -> [a] -> [a]
convolve xs ys
    = do
        x <- xs
        mult x <$> ys

nullPolynomial :: Polynomial a
nullPolynomial = Polynomial []

divEuclide :: Ring a => Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
divEuclide divided@(Polynomial a) divider@(Polynomial b)
    | degree divider < degree divided = (nullPolynomial, divided)
    | otherwise
        = undefined

mod_polynomial :: Ring a => Polynomial a -> Polynomial a -> Polynomial a
mod_polynomial a b = snd (divEuclide a b)

reduce :: Ring a => Polynomial a -> Polynomial a
reduce = mod_polynomial irreducible_m

irreducible_m :: Ring a => Polynomial a
irreducible_m = Polynomial [one, one, zero, one, one, zero, zero, zero, one]

add_polynomial :: Ring a => Polynomial a -> Polynomial a -> Polynomial a
add_polynomial (Polynomial a) (Polynomial b) = Polynomial (zipWithDefault add zero zero a b)

instance (Field a) => Ring (Polynomial a) where
    -- takes each coeff and adds them together; in case there's more coeffs on one side,
    -- it will just use `zero` to 'pad' the shorter one with zeroes
    add = add_polynomial
    zero = Polynomial []
    add_inverse (Polynomial a) = Polynomial (map add_inverse a)
    mult (Polynomial a) (Polynomial b) = Polynomial (convolve a b) `mod_polynomial` irreducible_m
    one = Polynomial [one]