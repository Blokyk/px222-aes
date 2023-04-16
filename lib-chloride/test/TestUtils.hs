{-# OPTIONS_GHC -Wno-missing-signatures #-}
module TestUtils (shouldBe, newTest, runTests) where

import GHC.Stack
import Data.List (partition)
import Control.Monad (unless)
import System.Exit (exitFailure)

import IOUtils

topFuncName stack = fst $ head $ getCallStack $ popCallStack stack
topFuncLoc  stack = snd $ head $ getCallStack stack

unexpectedVal :: (Show a) => a -> a -> String -> IO ()
unexpectedVal actual expected label =
    do
        putStrLn $ "    [" ++ bold (red "FAIL") ++ "] " ++ label
        putStr "        Expected: "
        putStrLn $ bold (show expected)
        putStr "        but got:  "
        putStrLn $ bold (show actual)

shouldBe :: (HasCallStack, Show a, Eq a) => a -> a -> IO ()
shouldBe value expected
    | value == expected = putStrLn $ green "OK"
    | otherwise
        = unexpectedVal value expected (topFuncName callStack ++ "(" ++ show line ++ "," ++ show col ++ ")")
    where line = srcLocStartLine $ topFuncLoc callStack
          col  = srcLocStartCol  $ topFuncLoc callStack

-- best thing here would just be to make some kind of "test harness" monad
-- instead of piggy-backing of `IO Bool` and doing all this stuff, but...
-- that'd be a bit overkill and i'm too lazy lol
newTest :: (HasCallStack, Show a, Eq a) => String -> [(a ,a)] -> IO Bool
newTest testName [] = do putStrLn $ bold $ " " ++ testName ++ ": "; return True
newTest testName tests =
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
            = unexpectedVal val expected (topFuncName callStack ++ " #" ++ show i) >> newline

runTests :: String -> [IO Bool] -> IO ()
runTests name tests =
    do
        putStrLn $ "BEGIN: " ++ bold name
        newline
        results <- sequence tests
        -- checks that every test was successful, otherwise crashes
        unless (and results) exitFailure
        newline

