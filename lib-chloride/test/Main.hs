import TestByte
import TestCipher
import TestPolynomial

main :: IO ()
main =
    do
        putStrLn ""
        testPolynomial
        testByte
        testCipher
        putStrLn ""
