{-# HLINT ignore "Use :" #-}
{-# HLINT ignore "Use drop" #-}
module Utils (
      zipWithDefault
    , withIndex
    , padLeft
    , sequenceF
    , Utils.showHex
    , rowMajorMatrix
    , columnMajorMatrix
) where

import Numeric (showHex)
import Data.List (mapAccumL, partition, foldl')
import Data.Tuple (swap)
import Cipher (nb)

-- Same as zipWith, but instead of stopping when either of the
-- lists are empty, it will use the corresponding default value
zipWithDefault :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWithDefault f a _ [] ys         = map (f a) ys
zipWithDefault f _ b xs []         = map (`f` b) xs
zipWithDefault f a b (x:xs) (y:ys) = (f x y) : (zipWithDefault f a b xs ys)

-- returns a list of couples, where in each couple, the first element is an item from the starting list, and the second is its index in said list.
withIndex :: [a] -> [(a, Int)]
withIndex l = zip l [0..]


padLeft :: Int -> a -> [a] -> [a]
padLeft n x xs = replicate (n - length xs) x ++ xs

-- | Applies a sequence of operations from an initial value
--
-- sequenceF [f, g, h] 0 = h(g(f(0)))
sequenceF :: [a -> a] -> a -> a
sequenceF = foldl (flip (.)) id
-- sequenceF fs x = foldl' (\x' f -> f x') x fs

showHex :: Integral a => a -> [Char]
showHex i = padLeft 2 '0' $ Numeric.showHex i ""

rowMajorMatrix :: Int -> [a] -> [[a]]
rowMajorMatrix n l = map (map fst) bla
    where
        indexed = withIndex l
        modIndexed = map (\(a, i) -> (a, i `mod` n)) indexed
        possibleIndices = [0..(n-1)]
        (_, bla) = mapAccumL filterRemaining modIndexed possibleIndices
        filterRemaining remaining idx =
            swap $ partition (\(_, i) -> i == idx) remaining

columnMajorMatrix :: Int -> [a] -> [[a]]
columnMajorMatrix _ [] = []
columnMajorMatrix n l  = row : columnMajorMatrix n rest
    where (row, rest) = splitAt n l

chunksOf :: [a] -> Int -> [[a]]
chunksOf [] _ = [[[]]]
chunksOf list 0 = [list]
chunksOf x n = chunk :chunksOf rest n
             where (chunk,rest) = splitAt n x 