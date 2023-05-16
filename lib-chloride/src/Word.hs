module Word (
      Word(Word)
    , word
    , wordFromList
    , wordFromInt
    , Word.asPolynomial
    , asBytes
    , Word.asInt
    , asByteTuple
    , wordMod
) where

import Prelude hiding (Word)

import Utils

import Byte
import Bit
import Algebra

-- just like Byte, we don't want the user to create arbitrary-length "words"
newtype Word = Word (Polynomial Byte) deriving Eq

instance Show Word where
    show w
        = unwords $ map (showHex . Byte.asInt) $ asBytes w

word :: Byte -> Byte -> Byte -> Byte -> Word
word hh hl lh ll = wordFromList [hh, hl, lh, ll]

-- | Makes a word from the given bytes, from MSB to LSB
wordFromList :: [Byte] -> Word
wordFromList bytes
    | length bytes > 4 = error "Can't make a word from more than 4 bytes!"
    | otherwise        = Word $ polynomial $ reverse bytes

wordFromInt :: Int -> Word
wordFromInt i = word
    (byteFromInt $ (i `div` 256 `div` 256 `div` 256) `mod` 256)
    (byteFromInt $ (i `div` 256 `div` 256) `mod` 256)
    (byteFromInt $ (i `div` 256) `mod` 256)
    (byteFromInt $ i `mod` 256)

-- | Extracts the bytes of a word into a list, in big-endian order
asBytes :: Word -> [Byte]
asBytes (Word p) = padLeft 4 zero $ reverse $ coeffs p

asPolynomial :: Word -> Polynomial Byte
asPolynomial (Word p) = p

asInt :: Word -> Int
asInt = sum . map (\(b, i) -> (256^i) * Byte.asInt b) . withIndex . asBytes

toList :: Int ->  [Int]
toList 0 = []
toList i = [if odd i then 1 else 0] ++ toList (div i 10)

toBool :: [Int] -> [Bool]
toBool [] = []
toBool(x:xs) | x== 0 = [False] ++ toBool xs
             | x== 1 = [True] ++ toBool xs

bcdBytes :: [Bool] -> [Bit]
bcdBytes [] = []
bcdBytes (x:xs) = [Bit x] ++ bcdBytes xs

-- bcdWord :: Int -> Word
-- bcdWord i =  wordFromList $ byte $ bcdBytes $ toBool $ toList i

-- | Extracts the bytes of a word into a tuple, in big-endian order
asByteTuple :: Word -> (Byte, Byte, Byte, Byte)
asByteTuple = extract . asBytes
    where extract [hh, hl, lh, ll] = (hh, hl, lh, ll) -- asBytes always adds padding
          extract _ = error "There was a word with more than 4 bytes inside >_>"

irrWord :: Word
--                            1                     x^4
irrWord = Word $ polynomial [one, zero, zero, zero, one]

wordMod :: Word -> Word -> Word
wordMod (Word a) (Word m) = Word $ a `polyMod` m

instance Ring Word where
    zero = Word zero
    one = Word one
    add (Word p) (Word q) = Word (p `add` q) `wordMod` irrWord
    add_inverse (Word p) = Word $ add_inverse p
    mult (Word p) (Word q) = Word (p `mult` q) `wordMod` irrWord