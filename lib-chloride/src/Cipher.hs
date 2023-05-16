{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Cipher where

import Prelude hiding (Word)
import Data.List (mapAccumL, transpose, intercalate)
import GHC.Stack
import Debug.Trace

import Utils

import Algebra
import Byte
import Word

type Column = Word
type Key = [Word]
data KeyData = KeyData { getKey :: Key, keyIteration :: Int } -- "why do we need this?" -> see the code for key expansion at the bottom of the file
type Block = [Column]

nb :: Int
nb = 4

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

showBlock :: [Byte] -> String
showBlock bytes
    = "    |" ++ intercalate "|\n    |" (map show $ bytesToRows bytes) ++ "|"

encrypt :: HasCallStack => [Byte] -> [Byte] -> [Byte]
encrypt key input
    | length input /= 4*nb = error $ "The input must have a size of 16 bytes (got " ++ show (length input) ++ " bytes instead)"
    | not $ isLegalKey key = error $ "The key must have a size of 16, 24 or 32 bytes (got " ++ show (length key) ++ " bytes instead)"
    | otherwise = trace ("Encrypting:\n" ++ showBlock input ++ "\nwith key:\n" ++ showBlock key) $ cipher key input
    where
        isLegalKey key = case length key of
            16 -> True -- 128-bit
            24 -> True -- 196-bit
            32 -> True -- 256-bit
            _  -> False

cipher :: HasCallStack => [Byte] -> [Byte] -> [Byte]
cipher key input =
    concatMap asBytes $ cipherFunc key' (bytesToColumns input)
    where
        key' = KeyData (bytesToColumns key) 0

cipherFunc :: HasCallStack => KeyData -> Block -> Block
cipherFunc key =
        sequenceF [addRoundKey key, cipherLoop 1 $ nextKey key]
    where
        cipherLoop :: HasCallStack => Int -> KeyData -> Block -> Block
        cipherLoop i key
            -- in case it's the last round, we don't want to call MixColumns nor do
            -- any recursive stuff; instead of doing an if/else inside the list, we
            -- just split the cases here for simplicity
            | i == nr key = sequenceF [
                subBytes,
                shiftRows,
                addRoundKey key
            ]
            | otherwise = sequenceF [
                subBytes,
                shiftRows,
                mixColumns,
                addRoundKey key,
                cipherLoop (i+1) newKey
            ]
            where newKey = nextKey key

-- §5.1.1 / SubBytes

subWord :: HasCallStack => Column -> Column
subWord word = wordFromList $ map subByte $ asBytes word
subBytes :: HasCallStack => Block -> Block
subBytes = map subWord

subByte :: HasCallStack => Byte -> Byte
subByte b = byte (reverse $ map subBit $ withIndex bits) `add` c
    where
        -- b(i) + b(i + 4 % 8) + b(i + 5 % 8) + b(i + 6 % 8) + b(i + 7 % 8) + c(i)
        subBit (bi, i) = foldr add zero
            [ bi
            , bits !! ((i + 4) `mod` 8)
            , bits !! ((i + 5) `mod` 8)
            , bits !! ((i + 6) `mod` 8)
            , bits !! ((i + 7) `mod` 8)
            ]
        c = bcdByte 01100011
        bits = reverse $ asBits $ mult_inverse b

-- §5.1.2 / ShiftRows

rotWordLeft :: HasCallStack => Word -> Word
rotWordLeft w = word hl lh ll hh
    where (hh, hl, lh, ll) = asByteTuple w

shiftRows :: HasCallStack => Block -> Block
-- Basically, we're just doing: transposeBlock [r0, rotWord r1, rotWord $ rotWord r2, rotWord $ rotWord $ rotWord r3]
shiftRows cols = transposeBlock $ map (\(r, i) -> iterate rotWordLeft r !! i) $ withIndex rows
    where
        rows = transposeBlock cols

-- §5.1.3 / MixColumns

mixColumns :: HasCallStack => Block -> Block
mixColumns = map (mult $ wordFromInt 0x01010302)
    -- map f
        -- where
        --     f w = word s0' s1' s2' s3' where
        --         (s0, s1, s2, s3) = asByteTuple w
        --         s0' = (bcdByte 10 `mult` s0) `add` (bcdByte 11 `mult` s1) `add` s2 `add` s3
        --         s1' = (bcdByte 10 `mult` s1) `add` (bcdByte 11 `mult` s2) `add` s3 `add` s0
        --         s2' = (bcdByte 10 `mult` s2) `add` (bcdByte 11 `mult` s3) `add` s0 `add` s1
        --         s3' = (bcdByte 10 `mult` s3) `add` (bcdByte 11 `mult` s0) `add` s1 `add` s2

invMixColumns :: HasCallStack => Block -> Block
invMixColumns = map (mult $ wordFromInt 0x0e0b0d09)

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
nextKey (KeyData key iteration) =
    trace ("Generating key #" ++ show (iteration+1) ++ "... " ++ show (getKey res) ++ "\n") res
    where
        res = KeyData newKey (iteration+1)
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