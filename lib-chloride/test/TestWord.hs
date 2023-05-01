module TestWord(testWord) where

import Prelude hiding (null)

import Algebra
import Word
import Utils

testWord :: IO()
testPolynomial = runTests "Word" []

test :: HasCallStack => IO Bool
test = 
    newTest "" [
        
    ]