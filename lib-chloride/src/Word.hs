module Word (
      Word()
    , word
    , wordFromList
    , asBytes
    , asByteTuple
) where

import Prelude hiding (Word)

import Byte
import Algebra

-- just like Byte, we don't want the user to create arbitrary-length "words"
newtype Word = Word (Polynomial Byte) deriving Eq

instance Show Word where
    show b
        = unwords $ map show paddedBits
        where bytes = asBytes b
              paddedBits = replicate (8 - length bytes) zero ++ bytes

word :: Byte -> Byte -> Byte -> Byte -> Word
word hh hl lh ll = wordFromList [hh, hl, lh, ll]

wordFromList :: [Byte] -> Word
wordFromList bytes
    | length bytes > 4 = error "Can't make a word from more than 4 bytes!"
    | otherwise        = Word $ polynomial $ reverse bytes

-- | Extracts the bytes of a word into a list, in big-endian order
asBytes :: Word -> [Byte]
asBytes (Word p) = coeffs p

-- | Extracts the bytes of a word into a tuple, in big-endian order
asByteTuple :: Word -> (Byte, Byte, Byte, Byte)
asByteTuple (Word p) = extract (coeffs p)
    where extract [hh, hl, lh, ll] = (hh,   hl,   lh,   ll  )
          extract     [hl, lh, ll] = (zero, hl,   lh,   ll  )
          extract         [lh, ll] = (zero, zero, lh,   ll  )
          extract             [ll] = (zero, zero, zero, ll  )
          extract               [] = (zero, zero, zero, zero)
          extract _ = error "There was a word with more than 4 bytes inside >_>"

instance Ring Word where
    add = undefined
    zero = undefined
    add_inverse = undefined
    mult = undefined
    one = undefined