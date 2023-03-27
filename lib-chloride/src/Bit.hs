module Bit where

import Algebra.Group

newtype Bit = Bit Bool deriving Eq

instance Group Bit where
    unit = Bit False
    inverse = id
    operation a b = Bit (a /= b) -- xor <=> !=
