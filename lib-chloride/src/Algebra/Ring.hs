module Algebra.Ring (
      Ring(..)
    , sub
) where

-- Ring (fr: Anneaux)
class (Eq a) => Ring a where
    infixl 6 `add`
    add :: a -> a -> a
    zero :: a
    add_inverse :: a -> a
    infixl 7 `mult`
    mult :: a -> a -> a
    one :: a

infixl 6 `sub`
-- | Subtracts two elements using the ring's additive
-- inverse operation
sub :: Ring a => a -> a -> a
sub a b = add a $ add_inverse b

instance Ring Int where
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