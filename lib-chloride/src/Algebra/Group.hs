module Algebra.Group (
    Group(..)
) where

-- Groups
class (Eq a) => Group a where
    unit :: a
    inverse :: a -> a
    operation :: a -> a -> a

instance Group Integer where
    unit = 0
    inverse x = -x
    operation a b = a + b

instance Group Float where
    unit = 1.0
    inverse x = 1/x
    operation a b = a * b