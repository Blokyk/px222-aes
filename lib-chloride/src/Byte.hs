module Byte (
      Byte(Byte)
    , byteFromPolynomial
    , byteFromInt
    , bcdByte
    , byte
    , asBits
    , asPolynomial
    , asInt
    , xtime
    , rotLeft
    , byteMod
    , irreducibleByte
) where

import Bit
import Algebra
import Utils (padLeft)

import Data.Maybe (fromJust)
import Data.List (uncons)

-- we don't wanna export the constructor cause its kinda
-- unwieldy and it means users could make "bytes" of any length
newtype Byte = Byte (Polynomial Bit) deriving Eq

-- | Creates a byte from a 'Polynomial Bit' value, assuming @degree(P) < 8@
byteFromPolynomial :: Polynomial Bit -> Byte
byteFromPolynomial = byte . coeffs

byteFromInt :: Int -> Byte
byteFromInt i
    | i > 255   = error $ "Integer " ++ show i ++ " is too big to be converted into a byte"
    | otherwise = byte $ bits i
        where
            bits :: Int -> [Bit]
            bits 0  = [zero]
            bits 1  = [one]
            bits n  = bits q ++ [if r == 0 then zero else one]
                where (q, r) = quotRem n 2

-- | Creates a byte from an Integer describing it in BCD notation (Binary-Coded Decimal notation)
--
-- Example: @bcdByte 00011101 = 0x1C = byte [zero, zero, zero, one, one, one, zero, one]@
bcdByte :: Int -> Byte
bcdByte 0 = zero
bcdByte i = add (byte [if odd i then one else zero]) $ xtime (bcdByte (i `div` 10))

-- | Creates a byte from a list of bits in big-endian order (MSB first, LSB second)
-- WARNING: This function will error-out when @length bits > 8@
byte :: [Bit] -> Byte
byte bits
    | length bits > 8 = error "Can't make a byte from more than 8 bits!"
    | otherwise       = Byte $ polynomial $ reverse bits

-- | Decompose a byte into a list of bits, in big-endian order
asBits :: Byte -> [Bit]
asBits (Byte bits) = padLeft 8 zero $ reverse $ coeffs bits

asPolynomial :: Byte -> Polynomial Bit
asPolynomial (Byte p) = p

asInt :: Byte -> Int
asInt (Byte p) = applyPolynomial (\b i -> if asBool b then i else 0) p 2

-- | The byte used in 'xtime'
xbyte :: Byte
xbyte = byte [one, zero]

-- | Multiplies a byte by @0x2@, effectively shifting it left (and reducing)
-- it, if necessary, by XORing it with @0x1B@ (cf §4.2.1 of FIPS-197)
xtime :: Byte -> Byte
xtime = mult xbyte

-- | Does a left shift that "wraps around," i.e. the
-- MSB becomes the LSB instead of being discarded
rotLeft :: Byte -> Byte
rotLeft b = byte $ bits ++ [b7]
    where
        -- extracts the MSB then the rest of the byte
        (b7, bits) = fromJust $ uncons $ asBits b

-- | The byte used to "reduce" multiplication results that exceed 255, i.e
-- that use more than 8 bits
irreducibleByte :: Byte
--                                   x^0  x^1        x^3  x^4                    x^8
irreducibleByte = Byte $ polynomial [one, one, zero, one, one, zero, zero, zero, one]

byteMod :: Byte -> Byte -> Byte
byteMod (Byte a) (Byte b) = Byte (a `polyMod` b)

instance Ring Byte where
    zero = Byte zero
    one  = Byte one
    add (Byte p) (Byte q)  = Byte (add p q) `byteMod` irreducibleByte
    add_inverse (Byte p)   = Byte (add_inverse p)
    mult (Byte p) (Byte q) = Byte (mult p q) `byteMod` irreducibleByte

instance Field Byte where
    mult_inverse (Byte b)
        | b == zero = zero
        | otherwise = Byte (modInv b $ asPolynomial irreducibleByte) `byteMod` irreducibleByte

instance Show Byte where
    show b
        = map (\bit -> if asBool bit then '1' else '0') paddedBits
        where bits = asBits b
              paddedBits = replicate (8 - length bits) zero ++ bits