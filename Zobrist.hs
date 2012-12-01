module Zobrist where

import Data.Random
import Data.Int
import Data.Array
import Data.Bits
import Control.Monad

import Data.Random.Source.DevRandom

import GoTypes


-- A record containing the Zobrist hash elements
data ZobristData = ZobristData {
    getPositionElements :: Array (Int, Int, Player) Int64, -- Elements refering to specific positions on the board
    getPlayFlag :: Int64 -- An element representing black-to-play
} deriving (Show)

type ZobristHash = Int64

newZobristData :: Int -> RVar ZobristData
newZobristData n = do
    let elementsRange = ((1, 1, Black), (n, n, Neither))
    elements <- replicateM (rangeSize elementsRange) (uniform lower upper)
    playFlag <- uniform lower upper
    return $ ZobristData (listArray elementsRange elements) playFlag
  where
    lower = (minBound :: Int64)
    upper = (maxBound :: Int64)

emptyBoardHash :: ZobristData -> ZobristHash
emptyBoardHash (ZobristData elems flag) = flag `xor` (foldr (xor . (elems !)) 0 $ filter ((\(x, y, p) -> p == Neither)) (indices elems))

-- Change a point from the color of the first player to the color of the second player
changePoint :: Point -> Player -> Player -> ZobristData -> ZobristHash -> ZobristHash
changePoint pt current new (ZobristData elems _) hash = hash `xor` (elems ! (fst pt, snd pt, current)) `xor` (elems ! (fst pt, snd pt, new))

-- Toggle whether the black-to-play flag is set
toggleToPlay :: ZobristData -> ZobristHash -> ZobristHash
toggleToPlay (ZobristData _ flag) current = current `xor` flag
