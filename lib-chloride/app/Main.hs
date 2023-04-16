module Main (main) where

import Algebra
import Bit

p :: Polynomial Bit
-- p = 1 + X + X^2 + X^4 + X^6
p = polynomial [one, one, one, zero, one, zero, one]

q :: Polynomial Bit
-- q = 1 + X + X^7
q = polynomial [one, one, zero, zero, zero, zero, zero, one]

m :: Polynomial Bit
-- m = 1 + X + X^3 + X^4 + X^8
m = polynomial [one, one, zero, one, one, zero, zero, zero, one]

main :: IO ()
main
    = do
        print (mult p q)
        print (m `polyMod` p)