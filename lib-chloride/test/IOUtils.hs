module IOUtils where

red str = "\x1b[31m" ++ str ++ "\x1b[0m"
green str = "\x1b[32m" ++ str ++ "\x1b[0m"
bold str = "\x1b[1m" ++ str ++ "\x1b[0m"
italics str = "\x1b[3m" ++ str ++ "\x1b[0m"

newline = putStrLn ""