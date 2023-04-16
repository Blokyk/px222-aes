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

byteFromPolynomial :: Polynomial Bit -> Byte
byteFromPolynomial (Polynomial a) = byte a

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
irreducibleByte = Polynomial [one, one, zero, one, one, zero, zero, zero, one]

instance Ring Byte where
    zero = Byte zero
    one  = Byte one
    add (Byte p) (Byte q)  = Byte (add p q)
    add_inverse (Byte p)   = Byte (add_inverse p)
    mult (Byte p) (Byte q) = Byte (multPolynomial p q `polyMod` irreducibleByte)