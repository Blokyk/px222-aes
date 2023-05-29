module TestCipher(testCipher) where

import Runner

import Byte
import Cipher
import Word

testCipher :: HasCallStack => IO ()
testCipher =
    runTests "Cipher" [
        testSubBytes,   testInvSubBytes,
        testShiftRows,  testInvShiftRows,
        testMixColumns, testInvMixColumns,
        testAddRoundKey, testECB, testCBC
    ]

testSubBytes :: HasCallStack => IO Bool
testSubBytes = newTest "SubByte" $
    zip (map (subByte . byteFromInt) [0x0..0xff]) sbox

testInvSubBytes :: HasCallStack => IO Bool
testInvSubBytes = newTest "InvSubByte" $
    zip (map (invSubByte . byteFromInt) [0x0..0xff]) invSBox

testShiftRows :: HasCallStack => IO Bool
testShiftRows
    = newTest "ShiftRows" [
           (
            shiftRows $ map wordFromInt [0xd42711ae, 0xe0bf98f1, 0xb8b45de5, 0x1e415230],
                        map wordFromInt [0xd4bf5d30, 0xe0b452ae, 0xb84111f1, 0x1e2798e5]
        ), (
            shiftRows $ map wordFromInt [0x49ded289, 0x45db96f1, 0x7f39871a, 0x7702533b],
                        map wordFromInt [0x49db873b, 0x45395389, 0x7f02d2f1, 0x77de961a]
        )
    ]

testInvShiftRows :: HasCallStack => IO Bool
testInvShiftRows
    = newTest "InvShiftRows" [
           (
            invShiftRows $ map wordFromInt [0x01020304, 0x05060708, 0x090a0b0c, 0x0d0e0f00],
                           map wordFromInt [0x010e0b08, 0x05020f0c, 0x09060300, 0x0d0a0704]
        ), (
            invShiftRows $ map wordFromInt [0x00112233, 0x44556677, 0x8899aabb, 0xccddeeff],
                           map wordFromInt [0x00ddaa77, 0x4411eebb, 0x885522ff, 0xcc996633]
        )
    ]

testMixColumns :: HasCallStack => IO Bool
testMixColumns
    = newTest "MixColumns" [
           (
            mixColumns $ map wordFromInt [0xd4bf5d30, 0xe0b452ae, 0xb84111f1, 0x1e2798e5],
            map wordFromInt [0x046681e5, 0xe0cb199a, 0x48f8d37a, 0x2806264c]
        ), (
            mixColumns $ map wordFromInt [0x49db873b, 0x45395389, 0x7f02d2f1, 0x77de961a],
            map wordFromInt [0x584dcaf1, 0x1b4b5aac, 0xdbe7caa8, 0x1b6bb0e5]
        )
    ]

testInvMixColumns :: HasCallStack => IO Bool
testInvMixColumns
    = newTest "InvMixColumns" [
           (
            invMixColumns $ map wordFromInt [0xe9f74eec, 0x023020f6, 0x1bf2ccf2, 0x353c21c7],
                            map wordFromInt [0x54d990a1, 0x6ba09ab5, 0x96bbf40e, 0xa111702f]
        ),  (
            invMixColumns $ map wordFromInt [0xbaa03de7, 0xa1f9b56e, 0xd5512cba, 0x5f414d23],
                            map wordFromInt [0x3e1c22c0, 0xb6fcbf76, 0x8da85067, 0xf6170495]
        ), (
            invMixColumns $ map wordFromInt [0xc57e1c15, 0x9a9bd286, 0xf05f4be0, 0x98c63439],
                            map wordFromInt [0xb458124c, 0x68b68a01, 0x4b99f82e, 0x5f15554c]
        ), (
            invMixColumns $ map wordFromInt [0x9816ee74, 0x00f87f55, 0x6b2c049c, 0x8e5ad036],
                            map wordFromInt [0xe8dab690, 0x1477d465, 0x3ff7f5e2, 0xe747dd4f]
        ), (
            invMixColumns $ map wordFromInt [0xf4bcd454, 0x32e554d0, 0x75f1d6c5, 0x1dd03b3c],
                            map wordFromInt [0x36339d50, 0xf9b53926, 0x9f2c092d, 0xc4406d23]
        ), (
            invMixColumns $ map wordFromInt [0x5f726415, 0x57f5bc92, 0xf7be3b29, 0x1db9f91a],
                            map wordFromInt [0x6353e08c, 0x0960e104, 0xcd70b751, 0xbacad0e7]
        )
    ]

testAddRoundKey :: HasCallStack => IO Bool
testAddRoundKey
    = newTest "AddRoundKey" [
           (
            addRoundKey (KeyData (map wordFromInt [0x2b7e1516, 0x28aed2a6, 0xabf71588, 0x09cf4f3c]) 0) (map wordFromInt [0x3243f6a8, 0x885a308d, 0x313198a2, 0xe0370734]),
            map wordFromInt [0x193de3be, 0xa0f4e22b, 0x9ac68d2a, 0xe9f84808]
        ), (
            addRoundKey (KeyData (map wordFromInt [0xef44a541, 0xa8525b7f, 0xb671253b, 0xdb0bad00]) 0) (map wordFromInt [0x0fd6daa9, 0x603138bf, 0x6fc0106b, 0x5eb31301]),
            map wordFromInt [0xe0927fe8, 0xc86363c0, 0xd9b13550, 0x85b8be01]
        )
    ]

testECB :: HasCallStack => IO Bool
testECB
    = newTest "Encryption: ECB" [
           (
            encryptECB
                (block1 ++ block2)
                key,
            encrypt key block1 ++ encrypt key block2
        )
    ]
    where
        block1 = map byteFromInt [0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f]
        block2 = map byteFromInt [0xf0, 0xe1, 0xd2, 0xc3, 0xb4, 0xa5, 0x96, 0x87, 0x78, 0x69, 0x5a, 0x4b, 0x3c, 0x2d, 0x1e, 0x0f]
        key = map byteFromInt [0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff]

testCBC :: HasCallStack => IO Bool
testCBC
    = newTest "Encryption: CBC" [
           (
            encryptCBC
                (block1 ++ block2)
                key,
            encrypt key block1 ++ encrypt (encrypt key block1) block2
        )
    ]
    where
        block1 = map byteFromInt [0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f]
        block2 = map byteFromInt [0xf0, 0xe1, 0xd2, 0xc3, 0xb4, 0xa5, 0x96, 0x87, 0x78, 0x69, 0x5a, 0x4b, 0x3c, 0x2d, 0x1e, 0x0f]
        key = map byteFromInt [0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff]


sbox :: [Byte]
sbox = map byteFromInt
      [ 0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67,
        0x2b, 0xfe, 0xd7, 0xab, 0x76, 0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59,
        0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0, 0xb7,
        0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1,
        0x71, 0xd8, 0x31, 0x15, 0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05,
        0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75, 0x09, 0x83,
        0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29,
        0xe3, 0x2f, 0x84, 0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b,
        0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf, 0xd0, 0xef, 0xaa,
        0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c,
        0x9f, 0xa8, 0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc,
        0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2, 0xcd, 0x0c, 0x13, 0xec,
        0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19,
        0x73, 0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee,
        0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb, 0xe0, 0x32, 0x3a, 0x0a, 0x49,
        0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
        0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4,
        0xea, 0x65, 0x7a, 0xae, 0x08, 0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6,
        0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a, 0x70,
        0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9,
        0x86, 0xc1, 0x1d, 0x9e, 0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e,
        0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf, 0x8c, 0xa1,
        0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0,
        0x54, 0xbb, 0x16 ]

invSBox :: [Byte]
invSBox = map byteFromInt
    [
        0x52, 0x09, 0x6a, 0xd5, 0x30, 0x36, 0xa5, 0x38, 0xbf, 0x40, 0xa3, 0x9e, 0x81, 0xf3, 0xd7, 0xfb,
        0x7c, 0xe3, 0x39, 0x82, 0x9b, 0x2f, 0xff, 0x87, 0x34, 0x8e, 0x43, 0x44, 0xc4, 0xde, 0xe9, 0xcb,
        0x54, 0x7b, 0x94, 0x32, 0xa6, 0xc2, 0x23, 0x3d, 0xee, 0x4c, 0x95, 0x0b, 0x42, 0xfa, 0xc3, 0x4e,
        0x08, 0x2e, 0xa1, 0x66, 0x28, 0xd9, 0x24, 0xb2, 0x76, 0x5b, 0xa2, 0x49, 0x6d, 0x8b, 0xd1, 0x25,
        0x72, 0xf8, 0xf6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xd4, 0xa4, 0x5c, 0xcc, 0x5d, 0x65, 0xb6, 0x92,
        0x6c, 0x70, 0x48, 0x50, 0xfd, 0xed, 0xb9, 0xda, 0x5e, 0x15, 0x46, 0x57, 0xa7, 0x8d, 0x9d, 0x84,
        0x90, 0xd8, 0xab, 0x00, 0x8c, 0xbc, 0xd3, 0x0a, 0xf7, 0xe4, 0x58, 0x05, 0xb8, 0xb3, 0x45, 0x06,
        0xd0, 0x2c, 0x1e, 0x8f, 0xca, 0x3f, 0x0f, 0x02, 0xc1, 0xaf, 0xbd, 0x03, 0x01, 0x13, 0x8a, 0x6b,
        0x3a, 0x91, 0x11, 0x41, 0x4f, 0x67, 0xdc, 0xea, 0x97, 0xf2, 0xcf, 0xce, 0xf0, 0xb4, 0xe6, 0x73,
        0x96, 0xac, 0x74, 0x22, 0xe7, 0xad, 0x35, 0x85, 0xe2, 0xf9, 0x37, 0xe8, 0x1c, 0x75, 0xdf, 0x6e,
        0x47, 0xf1, 0x1a, 0x71, 0x1d, 0x29, 0xc5, 0x89, 0x6f, 0xb7, 0x62, 0x0e, 0xaa, 0x18, 0xbe, 0x1b,
        0xfc, 0x56, 0x3e, 0x4b, 0xc6, 0xd2, 0x79, 0x20, 0x9a, 0xdb, 0xc0, 0xfe, 0x78, 0xcd, 0x5a, 0xf4,
        0x1f, 0xdd, 0xa8, 0x33, 0x88, 0x07, 0xc7, 0x31, 0xb1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xec, 0x5f,
        0x60, 0x51, 0x7f, 0xa9, 0x19, 0xb5, 0x4a, 0x0d, 0x2d, 0xe5, 0x7a, 0x9f, 0x93, 0xc9, 0x9c, 0xef,
        0xa0, 0xe0, 0x3b, 0x4d, 0xae, 0x2a, 0xf5, 0xb0, 0xc8, 0xeb, 0xbb, 0x3c, 0x83, 0x53, 0x99, 0x61,
        0x17, 0x2b, 0x04, 0x7e, 0xba, 0x77, 0xd6, 0x26, 0xe1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0c, 0x7d
    ]