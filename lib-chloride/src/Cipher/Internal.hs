{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Cipher.Internal where

import Prelude hiding (Word)
import Data.List (mapAccumL)
import GHC.Stack

import Utils
import Cipher.Internal.Utils
import Cipher.Internal.Types

import Algebra
import Byte
import Word

cipher :: HasCallStack => [Byte] -> [Byte] -> [Byte]
cipher key input =
    concatMap asBytes $ cipherFunc (bytesToColumns key) (bytesToColumns input)

decipher :: HasCallStack => [Byte] -> [Byte] -> [Byte]
decipher key input =
    concatMap asBytes $ invCipherFunc (bytesToColumns key) (bytesToColumns input)

cipherFunc :: HasCallStack => Key -> Block -> Block
cipherFunc initialKey =
        sequenceF [addRoundKey initialKey, cipherLoop 1]
    where
        keys = keysFor initialKey
        roundCount = nr initialKey
        cipherLoop :: HasCallStack => Int -> Block -> Block
        cipherLoop i
            -- in case it's the last round, we don't want to call MixColumns nor do
            -- any recursive stuff; instead of doing an if/else inside the list, we
            -- just split the cases here for simplicity
            | i == roundCount = sequenceF [
                subBytes,
                shiftRows,
                addRoundKey rawKey
            ]
            | otherwise = sequenceF [
                subBytes,
                shiftRows,
                mixColumns,
                addRoundKey rawKey,
                cipherLoop (i+1)
            ]
            where rawKey = keys !! i

invCipherFunc :: HasCallStack => Key -> Block -> Block
invCipherFunc initialKey =
        sequenceF [addRoundKey (head keys), cipherLoop 1]
    where
        keys = reverse (take (roundCount + 1) $ keysFor initialKey)
        roundCount = nr initialKey
        cipherLoop :: HasCallStack => Int -> Block -> Block
        cipherLoop i
            -- in case it's the last round, we don't want to call MixColumns nor do
            -- any recursive stuff; instead of doing an if/else inside the list, we
            -- just split the cases here for simplicity
            | i == roundCount = sequenceF [
                invShiftRows,
                invSubBytes,
                addRoundKey rawKeys
            ]
            | otherwise = sequenceF [
                invShiftRows,
                invSubBytes,
                addRoundKey rawKeys,
                invMixColumns,
                cipherLoop (i+1)
            ]
            where rawKeys = keys !! i

-- §5.1.1 / SubBytes

subWord :: HasCallStack => Column -> Column
subWord = wordFromList . map subByte . asBytes
subBytes :: HasCallStack => Block -> Block
subBytes = map subWord

-- L'implémentation "pure" est au final beaucoup trop lente
-- pour être utilisée en pratique; elle est donc remplacée par
-- sa version plus "pragmatique," mais voilà son implémentation
-- originale pour postérité:
--
-- subByte :: Byte -> Byte
-- subByte b = applyPolynomial mult p b `byteMod` irreducibleByte
--     where
--         -- from "The Design of Rjindael (2002), Appendix C p.212"
--         -- This is the lagrange polynomial for the affine transform
--         -- (matrix + constant), and since we're operating over a finite
--         -- field, "interpolating" over every element is finding an exact
--         -- expression of the value
--         p = polynomial (
--                                        byteFromInt 0x63 :   -- degree 0
--                 replicate 126 zero ++ [byteFromInt 0x8f] ++ -- degree 127
--                 replicate 63 zero ++  [byteFromInt 0xb5] ++ -- degree 191
--                 replicate 31 zero ++  [byteFromInt 0x01] ++ -- degree 223
--                 replicate 15 zero ++  [byteFromInt 0xf4] ++ -- degree 239
--                 replicate 7 zero ++   [byteFromInt 0x25] ++ -- degree 247
--                 replicate 3 zero ++   [byteFromInt 0xf9] ++ -- degree 251
--                 replicate 1 zero ++   [byteFromInt 0x09] ++ -- degree 253
--                                       [byteFromInt 0x05]    -- degree 254
--             )

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

invSubWord :: HasCallStack => Column -> Column
invSubWord = wordFromList . map invSubByte . asBytes
invSubBytes :: HasCallStack => Block -> Block
invSubBytes = map invSubWord

invSubByte :: HasCallStack => Byte -> Byte
-- to be completely "pure," we should use a polynomial S(X) that's the reciprocal
-- of the P(X) used in SubByte(); however, S(X) has *a lot* of coefficients (256 to
-- be exact), which makes both typing it cumbersome and applying it very slow.
-- Instead, we only use a polynomial for the affine transform (p.37 of "The Design
-- of Rjindael" (2002)), and then invert the resulting byte; this is a lot faster
-- and involves a lot less coefficients than applying S(X) directly.
invSubByte b = mult_inverse ((p_f1 `valueAt` b) `byteMod` irreducibleByte)
    where
        -- found by doing lagrange interpolation on the table for f^-1
        -- given in Appendix C of "The Design of Rjindael" (2002)
        p_f1 = polynomial (
                                     [byteFromInt 0x05,    -- degree 0
                                      byteFromInt 0x05,    -- degree 1
                                      byteFromInt 0xfe] ++ -- degree 2
                replicate 1 zero ++  [byteFromInt 0x7f] ++ -- degree 4
                replicate 3 zero ++  [byteFromInt 0x5a] ++ -- degree 8
                replicate 7 zero ++  [byteFromInt 0x78] ++ -- degree 16
                replicate 15 zero ++ [byteFromInt 0x59] ++ -- degree 32
                replicate 31 zero ++ [byteFromInt 0xdb] ++ -- degree 64
                replicate 63 zero ++ [byteFromInt 0x6e]    -- degree 128
            )

-- §5.1.2 / ShiftRows

rotWordLeft :: HasCallStack => Word -> Word
rotWordLeft w = word hl lh ll hh
    where (hh, hl, lh, ll) = asByteTuple w

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

addRoundKey :: HasCallStack => Key -> Block -> Block
addRoundKey = zipWith add

-- §5.2 / KeyExpansion

-- R.I.P our previous, beautiful, on-demand-keys implementation :'(
-- Taken too early by the blight of split 192-bit keys and faulty tests

-- Since we only use keys in the cipher as groups of 4 words at a time,
-- we need a function to handle generation and "serialization" for 6- or 8-words
-- sized keys so that the cipher doesn't have to worry about it.
--
-- Note: This generates an infinite list of keys, so be careful what you do
-- with it, especially in strict functions!
keysFor :: HasCallStack => Key -> [Key]
keysFor initialKey
    | nk == 4 = keys
    | otherwise = chunksOf 4 $ concat keys
    where
        nk = length initialKey
        (_, keys) = mapAccumL (\prevKey i -> (keyAfter prevKey i, prevKey)) initialKey [0..]

-- The role of keyAfter is to generate the next key in the *schedule*, whereas nextKey
-- generates the next key that the cipher needs
keyAfter :: HasCallStack => Key -> Int -> Key
keyAfter key iteration = newKey
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

rcons :: HasCallStack => Int -> Word
-- rcons[0 >= i] = word [X^i, 0x0, 0x0, 0x0]
rcons i = wordFromList [iterate xtime one !! i, bcdByte 0, bcdByte 0, bcdByte 0]