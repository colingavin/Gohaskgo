module Utils where

import Data.Array

-- Tuples
pair :: a -> b -> (a, b)
pair a b = (a, b)

-- Lists
dropIndex :: [a] -> Int -> [a]
dropIndex xs n = beginning ++ (tail end)
  where
    (beginning, end) = splitAt n xs

-- Arrays
filterIndices :: Ix i => (e -> Bool) -> Array i e -> [i]
filterIndices f a = filter (f . (a !)) (indices a)
