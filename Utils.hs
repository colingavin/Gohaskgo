module Utils where

import Data.Array
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree

-- Tuples
pair :: a -> b -> (a, b)
pair a b = (a, b)

-- Lists
dropIndex :: [a] -> Int -> [a]
dropIndex xs n = beginning ++ (tail end)
  where
    (beginning, end) = splitAt n xs

count :: (a -> Bool) -> [a] -> Int
count f xs = foldr (\x curr -> if f x then curr + 1 else curr) 0 xs

dimensions :: [[a]] -> Maybe (Int, Int)
dimensions xss = do
    inner <- innerDim xss
    return (length xss, inner)
  where
    innerDim [] = Just 0
    innerDim xss | all (== (length $ head xss)) $ map length xss = Just $ length $ head xss
    innerDim _ = Nothing

-- Arrays
filterIndices :: Ix i => (e -> Bool) -> Array i e -> [i]
filterIndices f a = filter (f . (a !)) (indices a)

-- Sets
flattenSet :: (Ord a) => Set (Set a) -> Set a
flattenSet s = Set.foldr Set.union Set.empty s

-- Either
-- Unsafe function to extract the right value from an either, use when you *know*
-- it will never be Left (this is like fromJust in Data.Maybe)
fromRight :: Either a b -> b
fromRight (Right r) = r
fromRight _ = error "Attempt to extract Right value from Left."

-- Tree
singletonTree :: a -> Tree a
singletonTree t = return t
