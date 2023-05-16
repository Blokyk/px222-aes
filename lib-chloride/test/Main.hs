import TestByte
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
        testCipher
        putStrLn ""
