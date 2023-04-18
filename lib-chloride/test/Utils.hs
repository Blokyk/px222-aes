{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Utils (shouldBe, newTest, runTests, GHC.Stack.HasCallStack) where

import GHC.Stack
import Data.List (partition)
import Control.Monad (unless)
import System.Exit (exitFailure)

import IOUtils

topFuncName :: CallStack -> String
topFuncName stack = fst $ head $ getCallStack $ popCallStack stack
topFuncLoc :: CallStack -> SrcLoc
topFuncLoc  stack = snd $ head $ getCallStack stack

showValAndExpected :: (Show a) => a -> a -> IO ()
showValAndExpected actual expected =
    do
        putStr "        Expected: "
        putStrLn $ bold (show expected)
        putStr "        but got:  "
        putStrLn $ bold (show actual)

-- | Tests that two values are equal, and prints an error message if not
--
-- Unlike 'Control.Exception.assert', this will not crash, but simply
-- print a message detailing what value was expected, and what was actually
-- passed.
--
-- Example usage:
--
-- >>> shouldBe (sum [1, 8, -3]) 6
--   <interactive>(1, 1): OK
-- >>> shouldBe (filter even [5, 4, 2, 1]) [4, 2, 1]
--   <interactive>(1, 1): FAIL
--       Expected: [4, 2, 1]
--       but got:  [4, 2]
--
-- NOTE: Using this function as an infix operator can make tests sound
-- quite natural and avoid some unnecessary parentheses. For example:
--
-- > 1 + 1 `shouldBe` 2
shouldBe :: (HasCallStack, Show a, Eq a) => a -> a -> IO Bool
shouldBe value expected
    | value == expected = do putStrLn $ "  " ++ label ++ ": " ++ green "OK"; return True
    | otherwise
        = do
            putStrLn $ "  " ++ label ++ ": " ++ bold (red "FAIL")
            showValAndExpected value expected
            return False
    where label = topFuncName callStack ++ "(" ++ show line ++ ", " ++ show col ++ ")"
          line = srcLocStartLine $ topFuncLoc callStack
          col  = srcLocStartCol  $ topFuncLoc callStack

-- | Defines a collection of tests by specifying pairs of values that should be the same,
-- with the first being the actual value, and the second the expected value. It will return
-- 'True' if every test passed, otherwise 'False'.
--
-- NOTE: Because of the type system, the values inside the pairs/list all have to be the
-- same type. If this problematic, you might want to use 'shouldBe' instead, or split the
-- list in two.
--
-- This will compare the values inside the pair, and then print a message for each one
-- where the values differ, as well as the index of the pair that failed.
--
-- Example usage:
--
-- >>> newTest "Addition" [ (6 + 1, 7), (78 - 2, 76), (1 + 1, 5) ]
-- Addition: 2 OKs, 1 FAILs
--    [FAIL] <interactive> #3:
--        Expected: 5
--        but got:  2
--
-- This function is roughly equivalent to calling 'shouldBe' on each pair of values:
--
-- >>> 6 + 1 `shouldBe` 7
-- >>> 78 - 2 `shouldBe` 76
-- >>> 1 + 1 `shouldBe` 5
newTest :: (HasCallStack, Show a, Eq a) =>
    String ->   -- ^ The name of this collection of tests
    [(a ,a)] -> -- ^ A list of values that should be the same
    IO Bool     -- ^ Returns true if every test was successful
newTest testName [] = do putStrLn $ bold $ " " ++ testName ++ ": "; return True
newTest testName tests =
-- best thing here would just be to make some kind of "test harness" monad
-- instead of piggy-backing off `IO Bool` and doing all this stuff, but...
-- that'd be a bit overkill and i'm too lazy lol
    do
        putStr $ "  " ++ bold testName ++ ": "
        -- assign an index to each test to perform
        let n_tests = zipWith (curry (\(i, (val, expected)) -> (val, expected, i))) [(1 :: Int)..] tests
        -- separate each test on whether they've succeeded or not
        let (success, failures) = partition (\(val, expected, _) -> val == expected) n_tests
        putStr $ show (length success) ++ green " OKs" ++ ", "
        putStr $ show (length failures) ++ red " FAILs"
        newline
        mapM_ printFail failures
        return (null failures) -- if we have no failures, we're fine!
    where
        printFail (val, expected, i)
            = do
                putStrLn $ "    [" ++ bold (red "FAIL") ++ "] " ++ (topFuncName callStack ++ " #" ++ show i)
                showValAndExpected val expected
                newline

-- | Runs a list of tests defined using 'shouldBe' or 'newTest' calls
--
-- This method runs every test, then checks if any of them failed; if
-- that's the case, it will then fail/crash
--
-- Example usage:
--
-- > runTests "Prelude tests" [
-- >     "hello " ++ "world!" `shouldBe` "hello world!",
-- >     newTest "Lists" [
-- >         (intercalate [0] [[1, 2], [3, 4], [5, 6]], [1, 2, 0, 3, 4, 0, 5, 6]),
-- >         (map read ["12", "-9", "0"], [12, -9, 0])
-- >     ]
-- > ]
runTests ::
    String ->    -- ^ The name of the test suite
    [IO Bool] -> -- ^ The list of tests to run
    IO ()
runTests name tests =
    do
        putStrLn $ "BEGIN: " ++ bold name
        newline
        results <- sequence tests
        -- checks that every test was successful, otherwise crashes
        unless (and results) exitFailure
        newline

