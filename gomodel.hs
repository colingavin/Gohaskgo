{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}

module GoModel where

import Data.Array
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Error

import Utils


-- Go is played by two players, Black and White, some intersections on the board may be Neither
data Player = Black | White | Neither deriving (Eq, Show, Ord, Enum)

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

-- Get all the adjacent points of a given point on the specified sized board
adjacentPoints :: Int -> Point -> Set Point
adjacentPoints n pt = Set.fromList $ filter (allowedPoint n) $ map (addPoint pt) directions
    where directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]

-- Create empty board of a given size indexed by Points
emptyBoard :: Int -> Array Point Player
emptyBoard n = array ((1,1), (n, n)) [((x, y), Neither) | x <- [1..n], y <- [1..n]]

-- Chains are contigious sets of points
type Chain = Set Point

joinChains :: Set Chain -> Chain
joinChains = Set.foldr Set.union Set.empty

surroundingPoints :: Int -> Chain -> Set Point
surroundingPoints n ch = (Set.unions $ map (adjacentPoints n) (Set.toList ch)) Set.\\ ch

removeChain :: Chain -> Array Point Player -> Array Point Player
removeChain ch board = board // map (flip pair Neither) (Set.toList ch)


-- PlayErrors happen when a point cannot be played at
data PlayError = OccupiedPoint | KoViolation | Suicide | Other deriving (Show)

instance Error PlayError where
    noMsg = Other


-- Positions are the main representation of the state of a board
data Position = Position {
    getBoardSize :: Int,
    getBoard :: Array Point Player,
    getBlackChains :: Set Chain,
    getWhiteChains :: Set Chain
} deriving (Eq, Show)

-- Create a new, empty position
emptyPosition :: Int -> Position
emptyPosition n = Position n (emptyBoard n) Set.empty Set.empty

-- Play at a point with a color (note, no pattern for Neither, this should be a crash)
positionByPlaying :: Player -> Point -> Position -> Either PlayError Position
positionByPlaying color pt pos@(Position n board bs ws)
    | not $ allowedPoint n pt = Left Other
    | board ! pt /= Neither = Left OccupiedPoint
    | color == White = Right $ Position n newBoard bs merged
    | color == Black = Right $ Position n newBoard merged ws
    where
        newBoard = board // [(pt, color)]
        -- Merge the chains that have pt as a liberty with pt and add them to the rest
        merged = Set.insert (Set.insert pt (joinChains withLiberty)) withoutLiberty
        -- Find the chains that don't have pt as a liberty
        withoutLiberty = (chainsForPlayer color pos) Set.\\ withLiberty
        -- Find the chains that have pt as a liberty
        withLiberty = chainsWithLiberty color pt pos

-- Create a new position by clearing all captured chains of a given color
positionByClearing :: Player -> Position -> Position
positionByClearing color pos@(Position n board bs ws)
    | color == White = Position n newBoard bs (ws Set.\\ capturedChains)
    | color == Black = Position n newBoard (bs Set.\\ capturedChains) ws
    | color == Neither = error "Can't clear empty."
  where
    capturedChains = Set.filter (\ch -> Set.null (libertiesOfChain color ch pos)) (chainsForPlayer color pos)
    newBoard = removeChain (joinChains capturedChains) board

-- Find all the chains of a specific color
chainsForPlayer :: Player -> Position -> Set Chain
chainsForPlayer White = getWhiteChains
chainsForPlayer Black = getBlackChains
-- Neither gives all chains
chainsForPlayer Neither = \pos -> Set.union (getWhiteChains pos) (getBlackChains pos)

-- Find all the liberties of a given chain
libertiesOfChain :: Player -> Chain -> Position -> Set Point
libertiesOfChain color ch (Position n board _ _) = Set.filter ((== Neither) . (board !)) $ surroundingPoints n ch

-- Determine which chains have a point as a liberty
chainsWithLiberty :: Player -> Point -> Position -> Set Chain
chainsWithLiberty color pt pos = Set.filter (\ch -> Set.member pt (libertiesOfChain color ch pos)) $ chainsForPlayer color pos

-- Find all the unoccupied points
allOfColor :: Player -> Position -> Set Point
allOfColor color (Position _ board _ _) = Set.fromList $ filterIndices (== color) board

-- Calculates the chinese score of the position for (Black, White)
scorePosition :: Position -> (Int, Int)
scorePosition pos@(Position n board bs ws) = (scoreColor Black, scoreColor White)
  where
    allPts = Set.fromList (indices board)
    emptyChains = emptyConnectedRegions [] (allOfColor Neither pos) pos
    scoreEmptyChains chs color = foldr ((+) . Set.size) 0 matchedChains
      where
        matchedChains = filter isInTerritory chs
        isInTerritory ch = Set.null $ Set.intersection (allOfColor (opponent color) pos) (surroundingPoints n ch)
    scoreColor color = scoreEmptyChains emptyChains color + Set.size (allOfColor color pos)

-- Inner method for score calculation: 
    -- [Chain] is the empty chains so far created 
    -- Set Point is the set of unconsidered empty points
emptyConnectedRegions :: [Chain] -> Set Point -> Position -> [Chain]
emptyConnectedRegions chs emp pos
    -- If there are no empty points to consider, just return what we have
    | Set.null emp = chs
    -- Otherwise, pick an arbitrary point in the empty set and expand it
    | otherwise = emptyConnectedRegions (expanded:chs) (emp Set.\\ expanded) pos
  where
    expanded = expandEmptyChain (Set.singleton (head (Set.toList emp))) pos

-- Take a chain consisting of empty points and expand it until it has no liberties
expandEmptyChain :: Chain -> Position -> Chain
expandEmptyChain ch pos
    | Set.null libs = ch
    | otherwise = expandEmptyChain (Set.union ch libs) pos
  where
    libs = libertiesOfChain Neither ch pos

-- Create a nice string representation of the position
prettyPrintPosition :: Position -> String
prettyPrintPosition (Position n board bs ws) = intercalate "\n" (map stringForRow [1 .. n])
  where
    stringForRow x = intersperse ' ' $ map (charForPoint x) [1 .. n]
    charForPoint x y = case board ! (x, y) of
        Black -> '●'
        White -> '○'
        Neither -> '+'

-- There are two different states that a game can be in, these are represented by different types

-- IncompleteGames retain their history and other information, they also can be played
data IncompleteGame = IncompleteGame {
    getHistory :: [Position],
    getToPlay :: Player
} deriving (Show)

makeNewGame :: Int -> IncompleteGame
makeNewGame n = IncompleteGame [emptyPosition n] Black

play :: IncompleteGame -> Point -> Either PlayError IncompleteGame
play (IncompleteGame hist color) pt = do
    newPos <- positionByPlaying color pt (head hist)
    let cleared = positionByClearing (opponent color) newPos
    let selfCleared = positionByClearing color cleared
    if selfCleared /= cleared
        then throwError Suicide
        else if (selfCleared `elem` hist) then throwError KoViolation
        else return $ IncompleteGame (cleared:hist) (opponent color)

pass :: IncompleteGame -> AnyGame
pass (IncompleteGame hist color) = do
    if length hist >= 2 && (head hist) == (head $ tail hist)
        then Left $ FinishedGame (head hist)
        else return $ IncompleteGame ((head hist):hist) (opponent color)

emptyPoints :: IncompleteGame -> Set Point
emptyPoints = (allOfColor Neither) . latestPosition

-- FinishedGames only keep their last position, they also can be scored
data FinishedGame = FinishedGame {
    getLastPosition :: Position
} deriving (Show)

winner :: FinishedGame -> Player
winner gm = case (uncurry compare . score) gm of
    LT -> White
    EQ -> Neither
    GT -> Black

score :: FinishedGame -> (Int, Int)
score = scorePosition . latestPosition

-- Synonmy for either finished games or incomplete games, returned by 'play' function
type AnyGame = Either FinishedGame IncompleteGame

-- Class representing what can be done with any kind of game
class Show a => Game a where
    latestPosition :: a -> Position
    getSize :: a -> Int
    getSize = getBoardSize . latestPosition

instance Game IncompleteGame where
    latestPosition = head . getHistory

instance Game FinishedGame where
    latestPosition = getLastPosition

-- Make AnyGame an instance for convenience
instance Game AnyGame where
    latestPosition (Left f) = latestPosition f
    latestPosition (Right i) = latestPosition i
