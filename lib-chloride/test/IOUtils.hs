module IOUtils where

red :: String -> String
red str = "\x1b[31m" ++ str ++ "\x1b[0m"
green :: String -> String
green str = "\x1b[32m" ++ str ++ "\x1b[0m"
bold :: String -> String
bold str = "\x1b[1m" ++ str ++ "\x1b[0m"
italics :: String -> String
italics str = "\x1b[3m" ++ str ++ "\x1b[0m"

newline :: IO ()
newline = putStrLn ""