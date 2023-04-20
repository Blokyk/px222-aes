module TestByte where

import Utils

import Algebra
import Byte

testByte :: HasCallStack => IO ()
testByte = runTests "Byte" [testCtor, testUtils, testAdd, testMult]

testCtor :: HasCallStack => IO Bool
testCtor =
    newTest "Constructors" [
            (
            asBits $ byte [],
            [zero, zero, zero, zero, zero, zero, zero, zero]
        ), (
            asBits $ byte [one, zero, one, one, zero],
            [zero, zero, zero, one, zero, one, one, zero]
        )
    ]

testUtils :: HasCallStack => IO Bool
testUtils =
    newTest "Utils" [
            (
            bcdByte 0,
            byte []
        ), (
            bcdByte 0110_1001,
            byte [zero, one, one, zero, one, zero, zero, one]
        ), (
            bcdByte 1111_1111,
            byte [one, one, one, one, one, one, one, one]
        ), (
            byteFromInt 0,
            byte []
        ), (
            byteFromInt 0x13,
            byte [zero, zero, zero, one, zero, zero, one, one]
        ), (
            byteFromInt 0xff,
            byte [one, one, one, one, one, one, one, one]
        )
    ]

testAdd :: HasCallStack => IO Bool
testAdd =
    newTest "Addition" [
            (
            add (bcdByte 0101_0111) (bcdByte 1000_0011),
            bcdByte 1101_0100
        ), (
            add (bcdByte 1101_0010) (bcdByte 0010_1101),
            bcdByte 1111_1111
        )
    ]

testMult :: HasCallStack => IO Bool
testMult =
    newTest "Multiplication" [
            (
            mult (bcdByte 0101_0111) (bcdByte 1000_0011),
            bcdByte 1100_0001
        ), (
            mult (bcdByte 0101_0111) (bcdByte 0001_0011),
            bcdByte 1111_1110
        ), (
            xtime (bcdByte 0010_1110),
            bcdByte 0101_1100
        ), (
            xtime (bcdByte 1001_1001),
            bcdByte 0010_1001 -- [1_]0011_0010 `xor` 0001_1011
        )
    ]