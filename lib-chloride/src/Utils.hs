module Utils (
      zipWithDefault
    , withIndex
    , padLeft
) where

-- Same as zipWith, but instead of stopping when either of the
-- lists are empty, it will use the corresponding default value
zipWithDefault :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWithDefault f a _ [] ys         = map (f a) ys
zipWithDefault f _ b xs []         = map (`f` b) xs
zipWithDefault f a b (x:xs) (y:ys) = (f x y) : (zipWithDefault f a b xs ys)

withIndex :: [a] -> [(a, Int)]
withIndex l = zip l [0..]

padLeft :: Int -> a -> [a] -> [a]
padLeft n x xs = replicate (n - length xs) x ++ xs