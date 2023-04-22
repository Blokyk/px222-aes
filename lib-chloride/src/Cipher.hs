module Cipher where

import Prelude hiding (Word)

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
