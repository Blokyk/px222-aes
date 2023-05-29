{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module TestPolynomial(testPolynomial) where

import Prelude hiding (quot, rem)

import Runner

import Algebra
import Bit

default (Float)

testPolynomial :: IO ()
testPolynomial = runTests "Polynôme" [testCtor, testAdd, testMult, testDiv, testMod, testApplyPolynomial, testDivProp]

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
            add zero zero,
            zero
        ), (
            add zero (polynomial [6, 4]),
            polynomial [6, 4]
        ), (
            add (polynomial [-3, 2]) zero,
            polynomial [-3, 2]
        ),

           (
            add (polynomial [1, 2, 3]) zero,
            polynomial [1, 2, 3]
        ), (
            add (polynomial [1, 8, 3, 9]) (polynomial [9, 2]),
            polynomial [10, 10, 3, 9]
        ),

           (
            add_inverse zero,
            zero
        ), (
            add_inverse (polynomial [8, 6, 1, 7]),
            polynomial [-8, -6, -1, -7]
        )
    ]

testMult :: HasCallStack => IO Bool
testMult =
    newTest "Multiplication" [
          (
            mult one zero,
            zero
        ), (
            mult one (polynomial [5, 0]),
            polynomial [5, 0]
        )
        , (
            mult zero (polynomial [1, 0, 8]),
            zero
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
            multScalaire 5 zero,
            zero
        ), (
            multScalaire 2 (polynomial [8, 7, 0, 5]),
            polynomial [16, 14, 0, 10]
        )
    ]

testDiv :: HasCallStack => IO Bool
testDiv =
    newTest "Division" [
           (
            divEuclide zero (polynomial [2, 1]) ,
            (zero, zero)
        ), (
            divEuclide (polynomial [3, 8, 0, 5]) (polynomial [1, 2]),
            (polynomial [4.625, -1.25, 2.5], polynomial [-1.625])
        )
    ]

testDivProp :: HasCallStack => IO Bool
testDivProp =
    newTest "DivisionProp" $ map (uncurry testPolyDiv) polyPairs
    where
        polys = [polynomial (rotate n l) | n <- [0..3], l <- [[one, one, zero], [one, one, zero, zero, one], [zero, one, one :: Bit]]]
        polyPairs = [(p, q) | p <- polys, q <- polys]
        testPolyDiv p q = ((rem `add` (quot `mult` q), q), (p, q))
            where (quot, rem) = p `divEuclide` q

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

testMod :: HasCallStack => IO Bool
testMod =
    newTest "Modulo" [
           (
            polyMod zero (polynomial [8, 2]),
            zero
        ), (
            polyMod (polynomial [0, 1, 5]) (polynomial [1, 5]),
            zero
        ), (
            polyMod (polynomial [-12, -5, -10, 3]) (polynomial [4, -1]),
            zero
        ), (
            polyMod (polynomial [3, 8, 0, 5]) (polynomial [1, 2]),
            polynomial [-1.625]
        ), (
            polyMod (polynomial [1, 8, 9, 3, 7]) (polynomial [-1, 0, 0, 1]),
            polynomial [4, 15, 9]
        )
    ]

testApplyPolynomial :: HasCallStack => IO Bool
testApplyPolynomial =
    newTest "applyPolynomial" [
           (
            applyPolynomial (*) (polynomial [1, 1, 5]) 2, -- 5*X^2 + X + 1 where X = 2
            23
        ), (
            applyPolynomial (*) (polynomial [1]) 10,
            1
        ), (
            applyPolynomial (*) (polynomial []) 3,
            0
        )
    ]