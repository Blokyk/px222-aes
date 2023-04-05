module Main (main) where

import Algebra.Polynomial
import Bit

p :: Polynomial Bit
p = Polynomial [one, zero, zero, one, one, one, one, zero, one, one, zero, one]

main :: IO ()
main = print (divEuclide p irreducible_m)
