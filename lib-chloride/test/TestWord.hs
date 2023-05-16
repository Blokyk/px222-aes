module TestWord(testWord) where

import Runner

import Byte
import Word
import Algebra

testWord :: HasCallStack => IO ()
testWord =
    runTests "Word" [
        testCtor,
        testAdd,
        testMult
    ]

testCtor :: HasCallStack => IO Bool
testCtor =
    newTest "Constructor" [
           (
            asBytes $ word (bcdByte 00) (bcdByte 01) (bcdByte 10) (bcdByte 11),
            [bcdByte 00, bcdByte 01, bcdByte 10, bcdByte 11]
        ), (
            asBytes $ word (bcdByte 00101101) (bcdByte 10010110) (bcdByte 10100101) (bcdByte 00010001),
            [bcdByte 00101101, bcdByte 10010110, bcdByte 10100101, bcdByte 00010001]
        ), (
            asBytes $ wordFromList [bcdByte 00, bcdByte 01, bcdByte 10, bcdByte 11],
            [bcdByte 00, bcdByte 01, bcdByte 10, bcdByte 11]
        ), (
            asBytes $ wordFromList [bcdByte 00101101, bcdByte 10010110, bcdByte 10100101, bcdByte 00010001],
            [bcdByte 00101101, bcdByte 10010110, bcdByte 10100101, bcdByte 00010001]
        ), (
            asBytes $ wordFromInt 0x00010203,
            [bcdByte 00, bcdByte 01, bcdByte 10, bcdByte 11]
        ), (
            asBytes $ wordFromInt 0x2d96a511,
            [bcdByte 00101101, bcdByte 10010110, bcdByte 10100101, bcdByte 00010001]
        )
    ]

testAdd :: HasCallStack => IO Bool
testAdd =
    newTest "Addition" [
          (
            zero `add` one,
            one
        ), (
            one `add` one,
            zero -- add = xor
        ), (
            wordFromInt 0xdead `add` wordFromInt 0xbeef,
            wordFromInt 0x6042
        ), (
            wordFromInt 0x01234567 `add` wordFromInt 0x89abcdef,
            wordFromInt 0x88888888
        ), (
            wordFromInt 0xabcdef00 `add` wordFromInt 0x543210ff,
            wordFromInt 0xffffffff
        )
    ]

testMult :: HasCallStack => IO Bool
testMult =
    newTest "Multiplication" [
          (
            zero `mult` one,
            zero
        ), (
            one `mult` one,
            one
        ), (
            wordFromInt 0x2 `mult` wordFromInt 0x123,
            wordFromInt 0x246
        ), (
            wordFromInt 0xbaadf00d `mult` wordFromInt 0x1337face,
            wordFromInt 0x597ba9a9 -- 0x80265859fb8f mod irr_word
        ), (
            wordFromInt 0xd4bf5d30 `mult` wordFromInt 0x01010302,
            wordFromInt 0x046681e5
        ), (
            wordFromInt 0xe0b452ae `mult` wordFromInt 0x01010302,
            wordFromInt 0xe0cb199a
        )
    ]
