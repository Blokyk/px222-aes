import TestByte
import TestCipherInternals
import TestCipher
import TestPolynomial
import TestWord

main :: IO ()
main =
    do
        putStrLn ""
        testPolynomial
        testByte
        testWord
        testCipherInternals
        testCipher
        putStrLn ""
