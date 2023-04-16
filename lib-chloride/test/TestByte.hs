module TestByte where

import Control.Exception

import Byte

ok :: IO ()
ok = putStrLn "OK"

testByte :: IO ()
testByte =
    do
        putStrLn "BEGIN: Byte"
        