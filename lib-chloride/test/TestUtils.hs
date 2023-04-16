{-# OPTIONS_GHC -Wno-missing-signatures #-}
module TestUtils (shouldBe, startTest) where

import GHC.Stack
import Data.List (partition)

red str = "\x1b[31m" ++ str ++ "\x1b[0m"
green str = "\x1b[32m" ++ str ++ "\x1b[0m"
bold str = "\x1b[1m" ++ str ++ "\x1b[0m"
italics str = "\x1b[3m" ++ str ++ "\x1b[0m"

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

startTest :: (HasCallStack, Show a, Eq a) => String -> [(a ,a)] -> IO ()
startTest testName [] = putStrLn $ bold $ "-- " ++ testName ++ ": "
startTest testName tests =
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
    where
        printFail (val, expected, i)
            = unexpectedVal val expected (topFuncName callStack ++ " #" ++ show i) >> newline
        newline = putStrLn ""

