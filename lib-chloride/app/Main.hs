{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main (main) where

import Word

import Cipher
import Cipher.Internal.Utils

import CipherUtils

main :: IO ()
main
    = do
        let block = concatMap (asBytes . wordFromInt) [0x3243f6a8, 0x885a308d, 0x313198a2, 0xe0370734]
        let key   = concatMap (asBytes . wordFromInt) [0x2b7e1516, 0x28aed2a6, 0xabf71588, 0x09cf4f3c]
        -- let block = concatMap (asBytes . wordFromInt) [0x00112233, 0x44556677, 0x8899aabb, 0xccddeeff]
        -- let key   = concatMap (asBytes . wordFromInt) [0x00010203, 0x04050607, 0x08090a0b, 0x0c0d0e0f]
        let res = encrypt key block
        a <- seq res (return $ length res)
        print a
        putStrLn "Result:"
        putStrLn $ showByteBlock res
        putStrLn "Putting it back into the cipher!"
        let res' = decrypt key res
        seq res' putStrLn $ showByteBlock res'