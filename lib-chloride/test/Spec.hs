import Control.Exception

import Algebra

main :: IO ()
main =
    do
        test_polynome

ok :: IO ()
ok = putStrLn "OK"

test_polynome :: IO ()
test_polynome
    = do
        putStrLn "BEGIN: Polynôme"

        assert (coeffs (polynomial [1, 8, 3]) == [1, 8, 3 :: Float]) ok
        assert (coeffs (polynomial [5, 4, 7, 0, 0]) == [5, 4, 7 :: Float]) ok

        assert (add zero_p zero_p == zero_p) ok
        assert (add (polynomial [1, 2, 3]) zero_p == polynomial [1, 2, 3]) ok
        assert (add (polynomial [1, 8, 3, 9]) (polynomial [9, 2]) == polynomial [10, 10, 3, 9 :: Float]) ok

        assert (add_inverse (polynomial [8, 6, 1, 7]) == polynomial [-8, -6, -1, -7 :: Float]) ok

        assert (mult zero_p (polynomial [1, 0, 8]) == zero_p) ok
        assert (mult (polynomial [4, -1]) (polynomial [-3, -2, -3]) == polynomial [-12, -5, -10, 3 :: Float]) ok
        assert (mult (polynomial [1, 3]) (polynomial [1, 3]) == polynomial [1, 6, 9 :: Float]) ok

        assert (polyMod (polynomial []) (polynomial [1, 2]) == zero_p) ok
        assert (polyMod (polynomial [1, 8, 9, 3, 7]) (polynomial [-1, 0, 0, 1]) == polynomial [8, 8, 9, 3 :: Float]) ok
    where zero_p = polynomial [] :: Polynomial Float