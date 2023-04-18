module TestPolynomial(testPolynomial) where

import Prelude hiding (null)

import Algebra hiding (polynomial)
import qualified Algebra.Polynomial (polynomial)
import Utils

-- makes testing a bit more brief, cause we don't need to specify a type each test
polynomial :: [Float] -> Polynomial Float
polynomial = Algebra.Polynomial.polynomial

null :: Polynomial Float
null = polynomial []

testPolynomial :: IO ()
testPolynomial = runTests "Polynôme" [testCtor, testAdd, testMult, testDiv, testMod]

testCtor :: HasCallStack => IO Bool
testCtor =
    newTest "Constructor & Utils" [
           (
            coeffs (polynomial [1, 8, 3]),
            [1, 8, 3]
        ), (
            coeffs (polynomial [5, 4, 7, 0, 0]),
            [5, 4, 7]
        )
    ]

testAdd :: HasCallStack => IO Bool
testAdd =
    newTest "Addition & Subtraction" [
           (
            add zero null,
            null
        ), (
            add zero (polynomial [6, 4]),
            polynomial [6, 4]
        ), (
            add (polynomial [-3, 2]) zero,
            polynomial [-3, 2]
        ),

           (
            add (polynomial [1, 2, 3]) null,
            polynomial [1, 2, 3]
        ), (
            add (polynomial [1, 8, 3, 9]) (polynomial [9, 2]),
            polynomial [10, 10, 3, 9]
        ),

           (
            add_inverse null,
            null
        ), (
            add_inverse (polynomial [8, 6, 1, 7]),
            polynomial [-8, -6, -1, -7]
        )
    ]

testMult :: HasCallStack => IO Bool
testMult =
    newTest "Multiplication" [
          (
            mult one null,
            null
        ), (
            mult one (polynomial [5, 0]),
            polynomial [5, 0]
        )
        , (
            mult null (polynomial [1, 0, 8]),
            null
        ), (
            mult (polynomial [1, 6])  one,
            polynomial [1, 6]
        ), (
            mult (polynomial [4, -1]) (polynomial [-3, -2, -3]),
            polynomial [-12, -5, -10, 3]
        ), (
            mult (polynomial [1, 3]) (polynomial [1, 3]),
            polynomial [1, 6, 9]
        )
        , (
            multScalaire 5 null,
            null
        ), (
            multScalaire 2 (polynomial [8, 7, 0, 5]),
            polynomial [16, 14, 0, 10]
        )
    ]

testDiv :: HasCallStack => IO Bool
testDiv =
    newTest "Division" [
           (
            divEuclide null (polynomial [2, 1]) ,
            (null, null)
        ), (
            divEuclide (polynomial [3, 8, 0, 5]) (polynomial [1, 2]),
            (polynomial [4.625, -1.25, 2.5], polynomial [-1.625])
        )
    ]

testMod :: HasCallStack => IO Bool
testMod =
    newTest "Modulo" [
           (
            polyMod null (polynomial [8, 2]),
            null
        ), (
            polyMod (polynomial [1, 8, 9, 3, 7]) (polynomial [-1, 0, 0, 1]),
            polynomial [4, 15, 9]
        ), (
            polyMod (polynomial [3, 8, 0, 5]) (polynomial [1, 2]),
            polynomial [-1.625]
        ), (
            polyMod (polynomial [-12, -5, -10, 3]) (polynomial [4, -1]),
            null
        )
    ]