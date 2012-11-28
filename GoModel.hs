{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}

module GoModel where

import Data.Array
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Error
import Debug.Trace (trace)

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

-- Chains are contigious sets of points
data Chain = Chain {
    getPoints :: Set Point,
    getLiberties :: Set Point,
    getNeighbors :: Set Point
} deriving (Show, Eq, Ord)

joinChains :: Set Chain -> Chain
joinChains = Set.foldr joinPair (Chain Set.empty Set.empty Set.empty)
  where
    joinPair (Chain ps ls ns) (Chain ps' ls' ns') = Chain (Set.union ps ps') (Set.union ls ls') (Set.union ns ns')

surroundingPoints :: Int -> Set Point -> Set Point
surroundingPoints n ps = (Set.unions $ map (adjacentPoints n) (Set.toList ps)) Set.\\ ps

libertiesOnBoard :: Int -> Set Point -> Array Point Player -> Set Point
libertiesOnBoard n ps board = Set.foldr Set.union Set.empty $ Set.map (\p -> Set.filter ((== Neither) . (board !)) (adjacentPoints n p)) ps

removeChain :: Chain -> Array Point Player -> Array Point Player
removeChain (Chain ps _ _) board = board // map (flip pair Neither) (Set.toList ps)


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
    | color == White = Right $ Position n newBoard (updateLiberties Black newBoard n bs) merged
    | color == Black = Right $ Position n newBoard merged (updateLiberties White newBoard n ws)
    where
        newBoard = board // [(pt, color)]
        -- Merge the chains that have pt as a liberty with pt and add them to the rest
        merged = Set.insert (insertPoint pt color board n (joinChains withLiberty)) withoutLiberty
        -- Find the chains that do and don't have pt as a liberty
        (withLiberty, withoutLiberty) = Set.partition (\(Chain _ ls _) -> Set.member pt ls) (chainsForPlayer color pos)

-- Insert a point into a chain and update its liberties
insertPoint :: Point -> Player -> Array Point Player -> Int -> Chain -> Chain
insertPoint p color board n (Chain ps _ ns)
    | Set.null ps = Chain newPoints newLiberties newNeighbors
    | otherwise = Chain newPoints newLiberties ((Set.union newNeighbors (Set.delete p ns)) Set.\\ ps)
  where
    newNeighbors = adjacentPoints n p
    newLiberties = (libertiesOnBoard n newPoints board)
    newPoints = Set.insert p ps

-- Update the liberties of the chains of a given color
updateLiberties :: Player -> Array Point Player -> Int -> Set Chain -> Set Chain
updateLiberties color board n chs = Set.map updateOne chs
  where
    updateOne (Chain ps _ ns) = Chain ps (Set.filter ((== Neither) . (board !)) ns) ns

-- Create a new position by clearing all captured chains of a given color
positionByClearing :: Player -> Position -> Position
positionByClearing color pos@(Position n board bs ws)
    | color == White = Position n newBoard (updateLiberties Black newBoard n bs) (ws Set.\\ capturedChains)
    | color == Black = Position n newBoard (bs Set.\\ capturedChains) (updateLiberties White newBoard n ws)
    | color == Neither = error "Can't clear empty."
  where
    capturedChains = Set.filter (\ch -> Set.null (getLiberties ch)) (chainsForPlayer color pos)
    newBoard = removeChain (joinChains capturedChains) board

-- Construct a position from a board in Array form
positionFromBoard :: Array Point Player -> Maybe Position
positionFromBoard board = playThrough (indices board) (emptyPosition $ fst $ snd $ bounds board)
  where
    playThrough [] pos = Just pos
    playThrough (x:xs) pos | (board ! x) == Neither = playThrough xs pos
    playThrough (x:xs) pos = case positionByPlaying (board ! x) x pos of
        Left _ -> Nothing
        Right pos' -> playThrough xs pos'

-- Find all the chains of a specific color
chainsForPlayer :: Player -> Position -> Set Chain
chainsForPlayer White = getWhiteChains
chainsForPlayer Black = getBlackChains
-- Neither gives all chains
chainsForPlayer Neither = \pos -> Set.union (getWhiteChains pos) (getBlackChains pos)

-- Determine which chains have a point as a liberty
chainsWithLiberty :: Player -> Point -> Position -> Set Chain
chainsWithLiberty color pt pos = Set.filter (\ch -> Set.member pt (getLiberties ch)) $ chainsForPlayer color pos

-- Find all the unoccupied points
allOfColor :: Player -> Position -> Set Point
allOfColor color (Position _ board _ _) = Set.fromList $ filterIndices (== color) board

-- Calculates the chinese score of the position for (Black, White)
scorePosition :: Position -> (Int, Int)
scorePosition pos@(Position n board bs ws) = (scoreColor Black, scoreColor White)
  where
    allPts = Set.fromList (indices board)
    emptyChains = emptyConnectedRegions [] (allOfColor Neither pos) pos
    scoreEmptyChains chs color = foldr ((+) . Set.size) 0 $ filter isInTerritory chs
      where
        isInTerritory ch = Set.null $ Set.intersection (allOfColor (opponent color) pos) (surroundingPoints n ch)
    scoreColor color = scoreEmptyChains emptyChains color + Set.size (allOfColor color pos)

-- Inner method for score calculation: 
    -- [Chain] is the empty chains so far created 
    -- Set Point is the set of unconsidered empty points
emptyConnectedRegions :: [Set Point] -> Set Point -> Position -> [Set Point]
emptyConnectedRegions chs emp pos
    -- If there are no empty points to consider, just return what we have
    | Set.null emp = chs
    -- Otherwise, pick an arbitrary point in the empty set and expand it
    | otherwise = emptyConnectedRegions (expanded:chs) (emp Set.\\ expanded) pos
  where
    expanded = expandEmptyChain (Set.singleton (head (Set.toList emp))) pos

-- Take a chain consisting of empty points and expand it until it has no liberties
expandEmptyChain :: Set Point -> Position -> Set Point
expandEmptyChain ch pos
    | Set.null libs = ch
    | otherwise = expandEmptyChain (Set.union ch libs) pos
  where
    libs = (libertiesOnBoard (getBoardSize pos) ch (getBoard pos)) Set.\\ ch

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
    getToPlay :: Player,
    getLastWasPass :: Bool
} deriving (Show)

makeNewGame :: Int -> IncompleteGame
makeNewGame n = IncompleteGame [emptyPosition n] Black False

makeGameFromPosition :: Position -> Player -> IncompleteGame
makeGameFromPosition pos toPlay = IncompleteGame [pos] toPlay False

play :: IncompleteGame -> Point -> Either PlayError IncompleteGame
play (IncompleteGame hist color _) pt = do
    newPos <- positionByPlaying color pt (head hist)
    let cleared = positionByClearing (opponent color) newPos
    let selfCapture = any (Set.null . getLiberties) $ Set.toList (chainsForPlayer color cleared)
    if selfCapture
        then throwError Suicide
        else if (cleared `elem` hist) then throwError KoViolation
        else return $ IncompleteGame (cleared:hist) (opponent color) False

pass :: IncompleteGame -> AnyGame
pass (IncompleteGame (h:hs) color p) = do
    if p
        then Left $ FinishedGame h
        else return $ IncompleteGame (h:h:hs) (opponent color) True

resign :: IncompleteGame -> FinishedGame
resign (IncompleteGame (h:hs) _ _) = FinishedGame h

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
