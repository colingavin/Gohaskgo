module Utils where

import Data.Array
import Data.Set (Set)
import qualified Data.Set as Set

-- Tuples
pair :: a -> b -> (a, b)
pair a b = (a, b)

-- Lists
dropIndex :: [a] -> Int -> [a]
dropIndex xs n = beginning ++ (tail end)
  where
    (beginning, end) = splitAt n xs

chooseFirst :: [Maybe a] -> Maybe a
chooseFirst [] = Nothing
chooseFirst (Nothing:xs) = chooseFirst xs
chooseFirst ((Just x):xs) = Just x

-- Arrays
filterIndices :: Ix i => (e -> Bool) -> Array i e -> [i]
filterIndices f a = filter (f . (a !)) (indices a)

-- Sets
flattenSet :: (Ord a) => Set (Set a) -> Set a
flattenSet s = Set.foldr Set.union Set.empty s
