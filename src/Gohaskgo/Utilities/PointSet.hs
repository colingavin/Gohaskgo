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
import qualified Prelude (elem, foldr, filter)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic.Mutable as MV
import Data.Bits
import Data.Word
import Data.List hiding (elem, null, union, intersect, filter, insert, delete, foldr, (\\))

import Debug.Trace (trace)


newtype PointSet = PointSet { getBits :: V.Vector Word32 } deriving (Eq, Ord)
type Point = (Int, Int)

-- Utility
instance Show PointSet where
    show ps = "fromList " ++ (show $ toList ps)

widthToBlocks :: Int -> Int
widthToBlocks n = ceiling ((fromIntegral n)^2 / 32)

blockForIndex :: Int -> Int
blockForIndex n = n `div` 32

offsetForIndex :: Int -> Int
offsetForIndex n = n `mod` 32

firstSetIndex :: Word32 -> Int
firstSetIndex n = ffs n 0
  where
    ffs n i | n .&. 1 == 0 = ffs (shiftR n 1) (i + 1)
            | otherwise = i

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
empty = PointSet . flip V.replicate 0 . widthToBlocks

singleton :: Int -> Point -> PointSet
singleton n pt = PointSet $ V.generate (widthToBlocks n) (\b -> if b == block then bit offset else 0)
  where 
    block = blockForIndex idx
    offset = offsetForIndex idx
    idx = pointToIndex pt

fromList :: Int -> [Point] -> PointSet
fromList n pts = PointSet $ V.generate (widthToBlocks n) blockFromList
  where
    blockFromList j = Prelude.foldr (\offset curr -> if (pointFromIndex (start + offset)) `Prelude.elem` pts then shiftL (curr .|. 1) 1 else shiftL curr 1) 0 [0..31]
      where start = 32*j

-- Access
elem :: Point -> PointSet -> Bool
elem p (PointSet ps) = testBit (ps V.! (blockForIndex idx)) (offsetForIndex idx)
  where idx = pointToIndex p

null :: PointSet -> Bool
null = (== 0) . V.sum . getBits

width :: PointSet -> Int
width = floor . sqrt . fromIntegral . (*32) . V.length . getBits

size :: PointSet -> Int
size = (V.foldr ((+) . popCount) 0) . getBits

someElement :: PointSet -> Maybe Point
someElement (PointSet ps) = do
    idx <- V.findIndex (/= 0) ps
    let block = ps V.! idx
    let offset = firstSetIndex block
    return $ pointFromIndex (32*idx + offset)

someSingleton :: PointSet -> PointSet
someSingleton ps = case someElement ps of
    Nothing -> empty (width ps)
    Just pt -> singleton (width ps) pt

toList :: PointSet -> [Point]
toList = foldr (:) []

-- Set operations
elementwise :: (Word32 -> Word32 -> Word32) -> PointSet -> PointSet -> PointSet
elementwise op (PointSet as) (PointSet bs) = PointSet $ V.generate (V.length as) (\n -> (as V.! n) `op` (bs V.! n))

union :: PointSet -> PointSet -> PointSet
union = elementwise (.|.)

intersection :: PointSet -> PointSet -> PointSet
intersection  = elementwise (.&.)

(\\) :: PointSet -> PointSet -> PointSet
(\\) = elementwise without
  where
    a `without` b = a .&. (complement b)

filter :: (Point -> Bool) -> PointSet -> PointSet
filter f ps = fromList (width ps) (Prelude.filter f $ toList ps)

foldr :: (Point -> a -> a) -> a -> PointSet -> a
foldr f a (PointSet ps) = V.ifoldr eachInBlock a ps
  where
    eachInBlock idx block curr = Prelude.foldr (\n c -> if testBit block n then f (pointFromIndex $ idx + n) c else c) curr [0..31]

-- Modification
insert :: Point -> PointSet -> PointSet
insert p s@(PointSet ps) = if p `elem` s then s
    else PointSet $ V.modify insertInto ps
  where 
    insertInto v = do
        let idx = pointToIndex p
        block <- MV.read v (blockForIndex idx)
        let modified = setBit block (offsetForIndex idx)
        MV.write v (blockForIndex idx) modified

delete :: Point -> PointSet -> PointSet
delete p s@(PointSet ps) = if not $ p `elem` s then s
    else PointSet $ V.modify removeFrom ps
  where 
    removeFrom v = do
        let idx = pointToIndex p
        block <- MV.read v (blockForIndex idx)
        let modified = clearBit block (offsetForIndex idx)
        MV.write v (blockForIndex idx) modified

