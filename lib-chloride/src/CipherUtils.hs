{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module CipherUtils where

import Prelude hiding (Word)
import Data.List (transpose, intercalate)
import GHC.Stack

import Utils

import Byte
import Word

type Column = Word
type Key = [Word]
data KeyData = KeyData { getKey :: Key, keyIteration :: Int } -- "why do we need this?" -> see the code for key expansion at the bottom of the file
type Block = [Column]

nb :: Int
nb = 4

-- return the number of times the key will be encrypted, depending on its length
nr :: KeyData -> Int
nr key = case length (getKey key) of
        4 -> 10 -- 128-bit key => 10 rounds
        6 -> 12 -- 192-bit key => 12 rounds
        8 -> 14 -- 256-bit key => 14 rounds
        _ -> undefined

bytesToColumns :: HasCallStack => [Byte] -> [Column]
bytesToColumns bytes = map wordFromList $ columnMajorMatrix 4 bytes

bytesToRows :: HasCallStack => [Byte] -> [Word]
bytesToRows bytes = map wordFromList $ rowMajorMatrix 4 bytes

transposeBlock :: HasCallStack => Block -> Block
transposeBlock = map wordFromList . transpose . map asBytes

showByteBlock :: [Byte] -> String
showByteBlock = showBlock . bytesToColumns

showBlock :: Block -> String
showBlock blk
    = "    |" ++ intercalate "|\n    |" (map show $ transposeBlock blk) ++ "|"

cutToCipherBlocks :: [Byte] -> [[Byte]]
cutToCipherBlocks = chunksOf (4*nb)