module GoTypes where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Array

-- Go is played by two players, Black and White, some intersections on the board may be Neither
data Player = Black | White | Neither deriving (Eq, Show, Ord, Enum, Ix)

-- Get the opposing Player
opponent :: Player -> Player
opponent White = Black
opponent Black = White
opponent Neither = Neither


-- Points represent intersections on the board
type Point = (Int, Int)

-- Utility to add two points together coordinant-wise
addPoint :: Point -> Point -> Point
addPoint (ax, ay) (bx, by) = (ax + bx, ay + by)

-- Utility to determine if a point is allowed on a given sized board
allowedPoint :: Int -> Point -> Bool
allowedPoint n (x, y) = (x > 0) && (y > 0) && (x <= n) && (y <= n)

-- Memoize the lists of adjacent points
adjacentPointsArrays :: [Array Point (Set Point)]
adjacentPointsArrays = map adjacentPointsArray [0..]
  where
    adjacentPointsArray n = array ((1, 1), (n, n)) $ [((x, y), adjacentPoints' n x y) | x <- [1..n], y <- [1..n]]
    adjacentPoints' n x y = Set.fromList $ filter (allowedPoint n) $ [(x, y + 1), (x + 1, y), (x, y - 1), (x - 1, y)]

-- Get all the adjacent points of a given point on the specified sized board
adjacentPoints :: Int -> Point -> Set Point
adjacentPoints n pt = (adjacentPointsArrays !! n) ! pt

-- Create empty board of a given size indexed by Points
emptyBoard :: Int -> Array Point Player
emptyBoard n = array ((1,1), (n, n)) [((x, y), Neither) | x <- [1..n], y <- [1..n]]
