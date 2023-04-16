module Bit (
      Bit(..)
    , asBool
) where

import Algebra.Group
import Algebra.Ring
import Algebra.Field

newtype Bit = Bit Bool deriving Eq

-- | True if the bit is 1, False if it's 0
asBool :: Bit -> Bool
asBool (Bit True) = True
asBool (Bit False) = False

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

instance Show Bit where
    show (Bit False) = "0"
    show (Bit True)  = "1"