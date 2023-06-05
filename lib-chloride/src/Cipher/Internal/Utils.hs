{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Cipher.Internal.Utils where

import Prelude hiding (Word)
import Data.List (transpose, intercalate)
import GHC.Stack

import Utils

import Byte
import Word

import Cipher.Internal.Types
import Algebra (Ring(add))

nb :: Int
nb = 4

-- return the number of times the key will be encrypted, depending on its length
nr :: Key -> Int
nr key = case length key of
        4 -> 10 -- 128-bit key => 10 rounds
        6 -> 12 -- 192-bit key => 12 rounds
        8 -> 14 -- 256-bit key => 14 rounds
        n -> error $ "Can't determine number of rounds for key of size " ++ show n

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

infixl 3 `xorBytes`
xorBytes :: [Byte] -> [Byte] -> [Byte]
xorBytes = zipWith add

isLegalKey :: [Byte] -> Bool
isLegalKey key = case length key of
    16 -> True -- 128-bit
    24 -> True -- 196-bit
    32 -> True -- 256-bit
    _  -> False