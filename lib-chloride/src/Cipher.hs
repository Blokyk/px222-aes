{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Cipher(
      encrypt
<<<<<<< HEAD
    , decrypt
    , encryptECB
    , encryptCBC
    , intsAsCipherData
) where

import Prelude hiding (Word)
import Data.List (mapAccumL)
import GHC.Stack

import Utils
import CipherUtils

import Algebra
import Byte
import Word

intsAsCipherData :: [Int] -> [Byte]
intsAsCipherData = map byteFromInt
=======
    , encryptECB
    , encryptCBC
    , decrypt
    , decryptECB
    , decryptCBC
    , intsAsCipherData
    , wordsAsCipherData
) where

import Prelude hiding (Word)
import GHC.Stack

import Byte
import Word

import Cipher.Internal
import Cipher.Internal.Utils

intsAsCipherData :: [Int] -> [Byte]
intsAsCipherData = map byteFromInt

wordsAsCipherData :: [Word] -> [Byte]
wordsAsCipherData = concatMap asBytes
>>>>>>> e3c5f2058a78a356984dc18b03128402aa6ced44

encrypt :: HasCallStack => [Byte] -> [Byte] -> [Byte]
encrypt key input
    | length input /= 4*nb = error $ "The input must have a size of 16 bytes (got " ++ show (length input) ++ " bytes instead)"
    | not $ isLegalKey key = error $ "The key must have a size of 16, 24 or 32 bytes (got " ++ show (length key) ++ " bytes instead)"
    | otherwise =
        cipher key input

encryptECB :: HasCallStack => [Byte] -> [Byte] -> [Byte]
encryptECB key input
    | length input `mod` (4*nb) /= 0 = error $ "The input must have a size that is a multiple of 16 bytes (got " ++ show (length input) ++ "bytes instead, i.e. " ++ show (length input `mod` (4*nb)) ++ " too many.)"
    | otherwise =
        concatMap (encrypt key) $ cutToCipherBlocks input

encryptCBC :: HasCallStack => [Byte] -> [Byte] -> [Byte]
encryptCBC key input
    | length input `mod` (4*nb) /= 0 = error $ "The input must have a size that is a multiple of 16 bytes (got " ++ show (length input) ++ "bytes instead, i.e. " ++ show (length input `mod` (4*nb)) ++ " too many.)"
    | otherwise =
        concat $ drop 1 $ foldl f [iv] $ cutToCipherBlocks input
    where
        iv = key
        f :: [[Byte]] -> [Byte] -> [[Byte]]
        f acc blk = acc ++ [encrypt key (prevOutput `xorBytes` blk)]
            where prevOutput = last acc

encryptECB :: HasCallStack => [Byte] -> [Byte] -> [Byte]
encryptECB key input
    | length input `mod` (4*nb) /= 0 = error $ "The input must have a size that is a multiple of 16 bytes (got " ++ show (length input) ++ "bytes instead, i.e. " ++ show (length input `mod` (4*nb)) ++ " too many.)"
    | otherwise =
        concatMap (encrypt key) $ cutToCipherBlocks input

encryptCBC :: HasCallStack => [Byte] -> [Byte] -> [Byte]
encryptCBC key input
    | length input `mod` (4*nb) /= 0 = error $ "The input must have a size that is a multiple of 16 bytes (got " ++ show (length input) ++ "bytes instead, i.e. " ++ show (length input `mod` (4*nb)) ++ " too many.)"
    | otherwise =
        concat $ foldl (\acc blk -> acc ++ [encrypt (last acc) blk]) [key] $ cutToCipherBlocks input

decrypt :: HasCallStack => [Byte] -> [Byte] -> [Byte]
decrypt key input
    | length input /= 4*nb = error $ "The input must have a size of 16 bytes (got " ++ show (length input) ++ " bytes instead)"
    | not $ isLegalKey key = error $ "The key must have a size of 16, 24 or 32 bytes (got " ++ show (length key) ++ " bytes instead)"
    | otherwise =
        decipher key input

<<<<<<< HEAD
rotWordRight :: HasCallStack => Word -> Word
rotWordRight w = word ll hh hl lh
    where (hh, hl, lh, ll) = asByteTuple w

shiftRows :: HasCallStack => Block -> Block
-- Basically, we're just doing: transposeBlock [r0, rotWord r1, rotWord $ rotWord r2, rotWord $ rotWord $ rotWord r3]
shiftRows cols = transposeBlock $ map (\(r, i) -> iterate rotWordLeft r !! i) $ withIndex rows
    where
        rows = transposeBlock cols

invShiftRows :: HasCallStack => Block -> Block
invShiftRows cols = transposeBlock $ map (\(r, i) -> iterate rotWordRight r !! i) $ withIndex rows
    where
        rows = transposeBlock cols

-- §5.1.3 / MixColumns

mixColumns :: HasCallStack => Block -> Block
mixColumns = map (mult $ wordFromInt 0x01010302)

invMixColumns :: HasCallStack => Block -> Block
invMixColumns = map (mult $ wordFromInt 0x090d0b0e)

-- §5.1.4 / AddRoundKey

addRoundKey :: HasCallStack => KeyData -> Block -> Block
addRoundKey (KeyData key _) = zipWith add key

-- §5.2 / KeyExpansion

rcons :: HasCallStack => Int -> Word
-- rcons[0 >= i] = word [X^i, 0x0, 0x0, 0x0]
rcons i = wordFromList [iterate xtime one !! i, bcdByte 0, bcdByte 0, bcdByte 0]

-- The way the "key schedule" is implemented here is a bit different from the spec.
--
-- Basically, instead of generating an enormous array of all the keys we'll need and
-- doing every operation (including the expansion!) based on that big array, we only
-- generate one key at a time, and call 'nextKey' each round to get the next one. This
-- is because the core loop of KeyExpansion() only really depends on the `Nk` last columns,
-- i.e. on the last key; thus, we don't need to generate all the keys at once: we only
-- need to know the previous key we made!
--
-- The "price" we have to pay is to wrap the key in a tiny type that tracks the current
-- iteration/round we're on, which is needed for `rcons` unfortunately. Personally, I
-- find that it's still a lot cleaner than generating a massive array and indexing into
-- it each round. The other option would be to use something like the 'State' monad, to
-- store the current and future keys, and "consume/pop" a key each round, but this
-- turned-out to be a bit overkill and just made the code more confusing in most places.
--
-- This has the side-effect of slightly changing the semantic of the 'i' variable from
-- the spec:
--
--      - first, we chose to use 'i' to store the "original index" of the column, i.e.
--        the index of the column we're basing our transformation on
--
--      - using that property and the fact that we only depend on the last key to make
--        the next one, we can always act as if we're "generating/expanding the first one"
--        which means that 0 <= i < Nk (this has the consequence of making any 'mod Nk'
--        completely useless)

nextKey :: HasCallStack => KeyData -> KeyData
nextKey (KeyData key iteration) = KeyData newKey (iteration+1)
    where
        nk  = length key
        w i = key !! i

        -- Since for i > 0, we need the column we generated just before, we use
        -- mapAccum to "remember it" between each call. For i = 0, the "previous
        -- column" is just the last column of the key, so we use w[nk-1] to start.
        (_, newKey) = mapAccumL transformWithAcc (w (nk-1)) $ withIndex key

        -- This just calls 'transformCol' and puts the result into a tuple
        -- The first item will be used as the accumulator (i.e. prevCol) value
        -- in mapAccumL, and the second is the actual result
        transformWithAcc prevCol (initialCol, idx)
            = (newCol, newCol)
            where newCol = transformCol prevCol (initialCol, idx)

        -- Given the column right before it, the column in the original key, and
        -- its index, this will apply the right operation on it, which changes
        -- depending on the index.
        --
        -- NOTE: unlike in the spec, here the index is that of the original column,
        --       not the one we're currently writing; thus, it will always have an
        --       offset of Nk compared to "what it should be" if you're thinking in
        --       in the terms of the column we're writing (like in the spec.)
        --
        -- If you're comparing with the spec, here's a little rosetta stone:
        --      - prevCol = w[i-1]        (using the 'i' from the spec, not the 0-based one in c')
        --      - initCol = w[i-Nk]       (using the 'i' from the spec, not the 0-based one in c')
        --
        transformCol prevCol (initCol, initIdx) = c' initIdx `add` initCol
            where
                -- c' allows us to apply the "special treatment" we need
                -- to the words where it's needed depending on their index.
                --
                -- if i = 0, then `c' = SubWord(RotWord(prevCol)) + RCons[iter]`
                c' 0 = subWord (rotWordLeft prevCol) `add` rcons iteration
                -- if (Nk = 8) and (i = 4), then we apply SubWord on the previous column first
                c' 4 | nk == 8 = subWord prevCol
                c' _ = prevCol
=======
decryptECB :: HasCallStack => [Byte] -> [Byte] -> [Byte]
decryptECB key input
    | length input `mod` (4*nb) /= 0 = error $ "The input must have a size that is a multiple of 16 bytes (got " ++ show (length input) ++ "bytes instead, i.e. " ++ show (length input `mod` (4*nb)) ++ " too many.)"
    | otherwise =
        concatMap (decrypt key) $ cutToCipherBlocks input

decryptCBC :: HasCallStack => [Byte] -> [Byte] -> [Byte]
decryptCBC key input
    | length input `mod` (4*nb) /= 0 = error $ "The input must have a size that is a multiple of 16 bytes (got " ++ show (length input) ++ "bytes instead, i.e. " ++ show (length input `mod` (4*nb)) ++ " too many.)"
    | otherwise =
        concatMap snd $ drop 1 $ foldl f [(iv, [])] $ cutToCipherBlocks input
    where
        iv = key
        f :: [([Byte], [Byte])] -> [Byte] -> [([Byte], [Byte])]
        f acc currBlock
            = acc ++ [(currBlock, prevInput `xorBytes` decrypt key currBlock)]
            where (prevInput, _) = last acc
>>>>>>> e3c5f2058a78a356984dc18b03128402aa6ced44
