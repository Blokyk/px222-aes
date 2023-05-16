{-# OPTIONS_GHC -Wno-unused-imports #-}
module Main (main) where

import Byte
import Cipher
import Word

main :: IO ()
main
    = do
        let block = concatMap (asBytes . wordFromInt) [0x3243f6a8, 0x885a308d, 0x313198a2, 0xe0370734]
        let key  = concatMap (asBytes . wordFromInt) [0x2b7e1516, 0x28aed2a6, 0xabf71588, 0x09cf4f3c]
        let res = encrypt key block
        a <- seq res (return $ length res)
        print a
        putStrLn "Result:"
        putStrLn $ showBlock res