module Algebra.Ring (
      Ring(..)
    , sub
) where

-- Ring (fr: Anneaux)
class (Eq a) => Ring a where
    add :: a -> a -> a
    zero :: a
    add_inverse :: a -> a
    mult :: a -> a -> a
    one :: a

-- | Subtracts two elements using the ring's additive
-- inverse operation
sub :: Ring a => a -> a -> a
sub a b = add a $ add_inverse b

instance Ring Integer where
    add = (+)
    zero = 0
    add_inverse a = -a
    mult = (*)
    one = 1

instance Ring Float where
    add = (+)
    zero = 0.0
    add_inverse x = -x
    mult = (*)
    one = 1.0