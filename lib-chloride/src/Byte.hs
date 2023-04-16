module Byte (
    byteFromPolynomial,
    byte,
    asBits,
    xtime,
) where

import Bit

import Algebra

-- we don't wanna export the constructor cause its kinda
-- unwieldy and means users could make "bytes" of any length
newtype Byte = Byte (Polynomial Bit) deriving Eq

instance Show Byte where
    show b
        = map (\(Bit bit) -> if bit then '1' else '0') $ reverse paddedBits
        where bits = asBits b
              paddedBits = bits ++ replicate (8 - length bits) (Bit False)

byteFromPolynomial :: Polynomial Bit -> Byte
byteFromPolynomial = byte . coeffs

byte :: [Bit] -> Byte
byte bits
    | length bits > 8 = error "Can't make a byte from more than 8 bits!"
    | otherwise       = Byte $ polynomial $ reverse bits

asBits :: Byte -> [Bit]
asBits (Byte bits) = coeffs bits

xbyte :: Byte
xbyte = byte [zero, one]

xtime :: Byte -> Byte
xtime = mult xbyte

irreducibleByte :: Field a => Polynomial a
--                          x^0  x^1        x^3  x^4                    x^8
irreducibleByte = polynomial [one, one, zero, one, one, zero, zero, zero, one]

instance Ring Byte where
    zero = Byte zero
    one  = Byte one
    add (Byte p) (Byte q)  = Byte (add p q)
    add_inverse (Byte p)   = Byte (add_inverse p)
    mult (Byte p) (Byte q) = Byte (mult p q `polyMod` irreducibleByte)