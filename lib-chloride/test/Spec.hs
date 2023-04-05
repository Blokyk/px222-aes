import Control.Exception

import Algebra.Polynomial
import Bit

main :: IO ()
main =
    do
        test_polynome

ok = putStrLn "OK"

test_polynome :: IO ()
test_polynome
    = do
        assert (add_polynomial (Polynomial []) (Polynomial [] :: Polynomial Bit) == Polynomial []) ok