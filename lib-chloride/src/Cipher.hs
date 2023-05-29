{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Use drop" #-}
module Cipher where

import Prelude hiding (Word)
import Data.List (mapAccumL, transpose, intercalate)
import GHC.Stack
import Debug.Trace

import Utils
    ( columnMajorMatrix, rowMajorMatrix, sequenceF, withIndex , chunksOf)

import Algebra
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

encrypt :: HasCallStack => [Byte] -> [Byte] -> [Byte]
encrypt key input
    | length input /= 4*nb = error $ "The input must have a size of 16 bytes (got " ++ show (length input) ++ " bytes instead)"
    | not $ isLegalKey key = error $ "The key must have a size of 16, 24 or 32 bytes (got " ++ show (length key) ++ " bytes instead)"
    | otherwise = trace ("Encrypting:\n" ++ showByteBlock input ++ "\nwith key:\n" ++ showByteBlock key) $
        cipher key input
    where
        isLegalKey key = case length key of
            16 -> True -- 128-bit
            24 -> True -- 196-bit
            32 -> True -- 256-bit
            _  -> False

decrypt :: HasCallStack => [Byte] -> [Byte] -> [Byte]
decrypt key input
    | length input /= 4*nb = error $ "The input must have a size of 16 bytes (got " ++ show (length input) ++ " bytes instead)"
    | not $ isLegalKey key = error $ "The key must have a size of 16, 24 or 32 bytes (got " ++ show (length key) ++ " bytes instead)"
    | otherwise = trace ("Decrypting:\n" ++ showByteBlock input ++ "\nwith key:\n" ++ showByteBlock key) $
        decipher key input
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

decipher :: HasCallStack => [Byte] -> [Byte] -> [Byte]
decipher key input =
    concatMap asBytes $ invCipherFunc key' (bytesToColumns input)
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
                cipherLoop (i+1) (nextKey key)
            ]

invCipherFunc :: HasCallStack => KeyData -> Block -> Block
invCipherFunc key =
        sequenceF [addRoundKey (head keys), cipherLoop 1 (keys !! 1)]
    where
        -- the way we handle keys is kinda catching backup to us here but... whatever, perf
        -- isn't important here so who cares
        -- reverse forces the whole list into WHNF, so we'll only compute the iterations once
        keys :: [KeyData]
        keys = reverse (take (nr key + 1) $ iterate nextKey key)
        cipherLoop :: HasCallStack => Int -> KeyData -> Block -> Block
        cipherLoop i key
            -- in case it's the last round, we don't want to call MixColumns nor do
            -- any recursive stuff; instead of doing an if/else inside the list, we
            -- just split the cases here for simplicity
            | i == nr key = sequenceF [
                invShiftRows,
                invSubBytes,
                addRoundKey key
            ]
            | otherwise = sequenceF [
                invShiftRows,
                invSubBytes,
                addRoundKey key,
                invMixColumns,
                cipherLoop (i+1) (keys !! (i+1))
            ]

-- §5.1.1 / SubBytes

subWord :: HasCallStack => Column -> Column
subWord word = wordFromList $ map subByte $ asBytes word
subBytes :: HasCallStack => Block -> Block
subBytes = map subWord

subByte :: Byte -> Byte
subByte b = applyPolynomial mult p b `byteMod` irreducibleByte
    where
        -- from "The Design of Rjindael (2002), Appendix C p.212"
        -- This is the lagrange polynomial for the affine transform
        -- (matrix + constant), and since we're operating over a finite
        -- field, "interpolating" over every element is finding an exact
        -- expression of the value
        p = polynomial (
                                       byteFromInt 0x63 :   -- degree 0
                replicate 126 zero ++ [byteFromInt 0x8f] ++ -- degree 127
                replicate 63 zero ++  [byteFromInt 0xb5] ++ -- degree 191
                replicate 31 zero ++  [byteFromInt 0x01] ++ -- degree 223
                replicate 15 zero ++  [byteFromInt 0xf4] ++ -- degree 239
                replicate 7 zero ++   [byteFromInt 0x25] ++ -- degree 247
                replicate 3 zero ++   [byteFromInt 0xf9] ++ -- degree 251
                replicate 1 zero ++   [byteFromInt 0x09] ++ -- degree 253
                                      [byteFromInt 0x05]    -- degree 254
            )

-- subByte :: HasCallStack => Byte -> Byte
-- subByte b = byte (reverse $ map subBit $ withIndex bits) `add` c
--     where
--         -- b(i) + b(i + 4 % 8) + b(i + 5 % 8) + b(i + 6 % 8) + b(i + 7 % 8) + c(i)
--         subBit (bi, i) = foldr add zero
--             [ bi
--             , bits !! ((i + 4) `mod` 8)
--             , bits !! ((i + 5) `mod` 8)
--             , bits !! ((i + 6) `mod` 8)
--             , bits !! ((i + 7) `mod` 8)
--             ]
--         c = bcdByte 01100011
--         bits = reverse $ asBits $ mult_inverse b

invSubWord :: HasCallStack => Column -> Column
invSubWord word = wordFromList $ map invSubByte $ asBytes word
invSubBytes :: HasCallStack => Block -> Block
invSubBytes = map invSubWord

invSubByte :: HasCallStack => Byte -> Byte
-- to be completely "pure," we should use a polynomial P1(X) that's the reciprocal
-- of the P(X) used in SubByte(); however, P1(X) has *a lot* of coefficients (256 to
-- be exact, just compose X^254 and p_f1, and then reduce cyclic/useless powers of X),
-- which makes both typing it cumbersome and applying it very slow.
-- Instead, we only use a polynomial for the affine transform (p.37 of "The Design
-- of Rjindael" (2002)), and then invert the resulting byte; this is a lot faster
-- and involves a lot less coefficients than applying P1(X) directly.
invSubByte b = mult_inverse (applyPolynomial mult p_f1 b `byteMod` irreducibleByte)
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


ecb_encrypt :: HasCallStack => [Byte] -> [Byte] -> [Byte]
ecb_encrypt key input
    | length input /= 4*nb = trace ("Encrypting:\n" ++ showByteBlock (block!!1) ++ "\nwith key:\n" ++ showByteBlock key) $
        cipher key (block !! 1)
    | not $ isLegalKey key = error $ "The key must have a size of 16, 24 or 32 bytes (got " ++ show (length key) ++ " bytes instead)"
    | otherwise = trace ("Encrypting:\n" ++ showByteBlock input ++ "\nwith key:\n" ++ showByteBlock key) $
        cipher key input
    where
        isLegalKey key = case length key of
            16 -> True -- 128-bit
            24 -> True -- 196-bit
            32 -> True -- 256-bit
            _  -> False
        block = chunksOf input 4

{- 
cbc_encrypt :: HasCallStack => [Byte] -> [Byte] -> [Byte]
cbc_encrypt key input
    | length input /= 4*nb =  trace ("Encrypting:\n" ++ showByteBlock input ++ "\nwith key:\n" ++ showByteBlock key) $
        cipher key block !! 1
            where block  = zipWith add prec key
                  prec = if block == ((chunksOf 4 input ) !! 1 ) then prec = init else prec = encrypt (chunksOf 4 input )
    | not $ isLegalKey key = error $ "The key must have a size of 16, 24 or 32 bytes (got " ++ show (length key) ++ " bytes instead)"
    | otherwise = trace ("Encrypting:\n" ++ showByteBlock input ++ "\nwith key:\n" ++ showByteBlock key) $
        cipher key input
    where
        isLegalKey key = case length key of
            16 -> True -- 128-bit
            24 -> True -- 196-bit
            32 -> True -- 256-bit
            _  -> False
        block  = zipWith add prec key
        prec = if block == ((chunksOf 4 input ) !! 1 ) then prec = init else prec = encrypt (chunksOf 4 input )
 -}