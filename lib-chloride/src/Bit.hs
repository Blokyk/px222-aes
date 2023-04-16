module Bit (one, zero, add, add_inverse, mult, Bit(Bit), asBool) where

import Algebra.Group
import Algebra.Ring
import Algebra.Field

newtype Bit = Bit Bool deriving Eq

asBool :: Bit -> Bool
asBool (Bit True) = True
asBool (Bit False) = False

instance Show Bit where
    show (Bit False) = "0"
    show (Bit True)  = "1"

instance Group Bit where
    unit = Bit False
    inverse = id
    operation a b = Bit (a /= b) -- xor <=> !=

instance Ring Bit where
    add = operation
    zero = Bit False
    add_inverse = id
    mult (Bit a) (Bit b) = Bit (a && b)
    one = Bit True

instance Field Bit where
    mult_inverse (Bit True) = Bit True
    mult_inverse _          = undefined