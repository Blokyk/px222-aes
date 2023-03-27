module Algebra.Field where

import Algebra.Ring

-- Field (fr: Corps)
class Ring a => Field a where
    mult_inverse :: a -> a

instance Ring Float where
    add = (+)
    unit_add = 0.0
    add_inverse x = -x
    mult = (*)
    unit_mult = 1.0

instance Field Float where
    mult_inverse x = 1/x