{-# LANGUAGE MultiParamTypeClasses #-}

module Algebra.Polynomial where

import Algebra.Field

class Field coeff => Polynomial t coeff where
