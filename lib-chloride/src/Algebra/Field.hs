module Algebra.Field where

import Algebra.Ring

-- Field (fr: Corps)
class Ring a => Field a where
    mult_inverse :: a -> a

diviser :: Field a => a -> a -> a
diviser a b = mult a $ mult_inverse b

instance Field Float where
    mult_inverse x = 1/x