{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Gohaskgo.Utilities.PointSet (
    PointSet,
    empty,
    singleton,
    fromList,
    elem,
    null,
    toList,
    union,
    intersection,
    (\\),
    filter,
    insert,
    delete,
    foldr,
    size,
    width,
    someElement,
    someSingleton
    ) where

import Prelude hiding (elem, filter, null, foldr)
import qualified Prelude (elem)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic.Mutable as MV
import Data.Bits
import Data.List hiding (elem, null, union, intersect, filter, insert, delete, foldr, (\\))

import Gohaskgo.Utilities.Bit

newtype PointSet = PointSet { getBits :: BitVector } deriving (Eq, Ord)
type Point = (Int, Int)

-- Functions implementing a bijection (Int, Int) <-> Int such that 1..n^2 represent (1,1)..(n,n)
-- Using these functions means that functions like `elem` don't need to know the size of the board
pointToIndex :: Point -> Int
pointToIndex (n, m)
 | n == m = n * (n - 1)
 | n > m = n^2 - m
 | n < m = m * (m - 2) + n

pointFromIndexList :: [Point]
pointFromIndexList = map findPoint [0..]
  where
    findPoint n = case compare n (l + l^2) of
        EQ -> (l + 1, l + 1)
        GT -> (l + 1, 1 - n + 2*l + l^2)
        LT -> (n - l^2 + 1, l + 1)
      where
        l = floor $ sqrt (fromIntegral n)

pointFromIndex :: Int -> Point
pointFromIndex = (pointFromIndexList !!)

-- Creation
empty :: Int -> PointSet
empty = PointSet . flip V.replicate False . (^2)

singleton :: Int -> Point -> PointSet
singleton n pt = PointSet $ V.generate (n^2) (== idx) where idx = pointToIndex pt

fromList :: Int -> [Point] -> PointSet
fromList n pts = PointSet $ V.generate (n^2) (\j -> (pointFromIndex j) `Prelude.elem` pts)

-- Access
elem :: Point -> PointSet -> Bool
elem p (PointSet ps) = ps V.! (pointToIndex p)

null :: PointSet -> Bool
null = not . V.or . getBits

width :: PointSet -> Int
width = floor . sqrt . fromIntegral . V.length . getBits

size :: PointSet -> Int
size = (V.foldr (\e c -> if e then c + 1 else c) 0) . getBits

someElement :: PointSet -> Maybe Point
someElement (PointSet ps) = findElement ((V.length ps) - 1)
  where
    findElement (-1) = Nothing
    findElement n = if ps V.! n then Just $ pointFromIndex n else findElement (n - 1)

someSingleton :: PointSet -> PointSet
someSingleton ps = case someElement ps of
    Nothing -> empty (width ps)
    Just pt -> singleton (width ps) pt

toList :: PointSet -> [Point]
toList = foldr (:) []

-- Set operations
union :: PointSet -> PointSet -> PointSet
union (PointSet as) (PointSet bs) = PointSet $ as .|. bs

intersection :: PointSet -> PointSet -> PointSet
intersection (PointSet as) (PointSet bs) = PointSet $ as .&. bs

(\\) :: PointSet -> PointSet -> PointSet
(PointSet as) \\ (PointSet bs) = PointSet $ as .&. (complement bs)

filter :: (Point -> Bool) -> PointSet -> PointSet
filter f (PointSet ps) = PointSet $ V.generate (V.length ps) (\n -> (ps V.! n) && (f . pointFromIndex) n)

foldr :: (Point -> a -> a) -> a -> PointSet -> a
foldr f a (PointSet ps) = V.ifoldr (\n el curr -> if el then f (pointFromIndex n) curr else curr) a ps

-- Modification
insert :: Point -> PointSet -> PointSet
insert p s@(PointSet ps) = if p `elem` s then s
    else PointSet $ V.modify (\v -> MV.write v (pointToIndex p) True) ps

delete :: Point -> PointSet -> PointSet
delete p s@(PointSet ps) = if not $ p `elem` s then s
    else PointSet $ V.modify (\v -> MV.write v (pointToIndex p) False) ps

-- Utility
instance Show PointSet where
    show ps = "fromList " ++ (show $ toList ps)
