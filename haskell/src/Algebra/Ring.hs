module Algebra.Ring where

-- Ring (fr: Anneaux)
class Ring a where
    add :: a -> a -> a
    unit_add :: a
    add_inverse :: a -> a
    mult :: a -> a -> a
    unit_mult :: a

instance Ring Integer where
    add = (+)
    unit_add = 0
    add_inverse a = -a
    mult = (*)
    unit_mult = 1