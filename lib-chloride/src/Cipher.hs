module Cipher where

import Prelude hiding (Word)

import Utils

import Algebra
import Byte
import Word

import State

type Column = Word
type CipherState = State [Column] ()

initCipherFrom :: [Byte] -> CipherState
initCipherFrom bytes
    | length bytes `mod` 4 /= 0 = error "The input array must have a length of 4*Nb"
    | otherwise = stateFrom $ bytesToWords bytes
    where
        bytesToWords [] = []
        bytesToWords (ll:lh:hl:hh:bs) = word hh hl lh ll : bytesToWords bs
        bytesToWords _ = undefined

subBytes :: [Column] -> [Column]
subBytes = map (wordFromList . map subByte . asBytes)

subByte :: Byte -> Byte
subByte b = byte (map subBit $ withIndex bytes) `add` c
    where
        c = bcdByte 01100011
        -- b(i) + b(i + 4 % 8) + b(i + 5 % 8) + b(i + 6 % 8) + b(i + 7 % 8) + c(i)
        subBit (bi, i) = foldr add zero
            [ bi
            , bytes !! ((i + 4) `mod` 8)
            , bytes !! ((i + 5) `mod` 8)
            , bytes !! ((i + 6) `mod` 8)
            , bytes !! ((i + 7) `mod` 8)
            ]
        bytes = reverse $ asBits b