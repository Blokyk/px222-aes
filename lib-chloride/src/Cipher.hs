{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Cipher(
      encrypt
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

decrypt :: HasCallStack => [Byte] -> [Byte] -> [Byte]
decrypt key input
    | length input /= 4*nb = error $ "The input must have a size of 16 bytes (got " ++ show (length input) ++ " bytes instead)"
    | not $ isLegalKey key = error $ "The key must have a size of 16, 24 or 32 bytes (got " ++ show (length key) ++ " bytes instead)"
    | otherwise =
        decipher key input

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