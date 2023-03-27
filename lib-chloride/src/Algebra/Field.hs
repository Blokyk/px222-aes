module Algebra.Field where

import Algebra.Ring

-- Field (fr: Corps)
class Ring a => Field a where
    mult_inverse :: a -> a

instance Field Float where
    mult_inverse x = 1/x