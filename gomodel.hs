module GoModel where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.List


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


-- The colors of the players

data Color = White | Black | Empty deriving (Eq, Show)

-- Get the opposing color
opposingColor :: Color -> Color
opposingColor White = Black
opposingColor Black = White
opposingColor Empty = Empty


-- Chains are sets of points which share liberties

type Chain = Set Point

joinChains :: Set Chain -> Chain
joinChains = Set.foldr Set.union Set.empty

surroundingPoints :: Int -> Chain -> Set Point
surroundingPoints n ch = (Set.unions $ map (adjacentPoints n) (Set.toList ch)) Set.\\ ch


-- Positions represent the state of the board

data Position = Position { size :: Int, blackChains :: Set Chain, whiteChains :: Set Chain } deriving (Show, Eq)

-- Create a new, empty position
emptyPosition :: Int -> Position
emptyPosition n = Position n  Set.empty Set.empty

-- Find all the chains of a specific color
chainsOfColor :: Color -> Position -> Set Chain
chainsOfColor White = whiteChains
chainsOfColor Black = blackChains
chainsOfColor Empty = \pos -> Set.union (whiteChains pos) (blackChains pos)

-- Collect all the points occupied by a given color
allOfColor :: Color -> Position -> Set Point
allOfColor color = joinChains . chainsOfColor color

-- Collect all the occupied positions
allOccupied :: Position -> Set Point
allOccupied pos = Set.union (allOfColor White pos) (allOfColor Black pos)

-- Find all the liberties of a given chain
libertiesOfChain :: Color -> Chain -> Position -> Set Point
libertiesOfChain color ch pos = (surroundingPoints (size pos) ch) Set.\\ allOfColor (opposingColor color) pos

-- Determine which chains have a point as a liberty
chainsWithLiberty :: Color -> Point -> Position -> Set Chain
chainsWithLiberty color pt pos = Set.filter (\ch -> Set.member pt (libertiesOfChain color ch pos)) (chainsOfColor color pos)

-- Create a new position by playing at a point
positionByPlaying :: Color -> Point -> Position -> Position
positionByPlaying color pt pos
    | not $ allowedPoint (size pos) pt = error "Cannot play that point."
    | color == White = Position (size pos) (blackChains pos) merged
    | color == Black = Position (size pos) merged (whiteChains pos)
    | otherwise = pos
    where 
        -- Merge the chains that have pt as a liberty with pt and add them to the rest
        merged = Set.insert (Set.insert pt (joinChains withLiberty)) withoutLiberty
        -- Find the chains that don't have pt as a liberty
        withoutLiberty = (chainsOfColor color pos) Set.\\ withLiberty
        -- Find the chains that have pt as a liberty
        withLiberty = chainsWithLiberty color pt pos

-- Create a new position by removing a given chain
positionByRemoving :: Color -> Chain -> Position -> Position
positionByRemoving White ch (Position n bs ws) = Position n bs (Set.delete ch ws)
positionByRemoving Black ch (Position n bs ws) = Position n (Set.delete ch bs) ws
positionByRemoving Empty _ pos = pos

-- Calculates the chinese score of the position for (Black, White)
scorePosition :: Position -> (Int, Int)
scorePosition pos = (scoreColor Black, scoreColor White)
  where
    scoreColor color = scoreEmptyChains emptyChains color + Set.size (allOfColor color pos)
    scoreEmptyChains chs color = foldr ((+) . Set.size) 0 matchedChains
      where
        matchedChains = filter isInTerritory chs
        isInTerritory ch = Set.null $ Set.intersection (allOfColor (opposingColor color) pos) (surroundingPoints (size pos) ch)
    emptyChains = scorePosition' [] (allPts Set.\\ allOccupied pos) pos
    allPts = Set.fromList ([(x,y) | x <- [1..(size pos)], y <- [1..(size pos)]] :: [(Int, Int)])

-- Inner method for score calculation: 
    -- [Chain] is the empty chains so far created 
    -- Set Point is the set of unconsidered empty points
scorePosition' :: [Chain] -> Set Point -> Position -> [Chain]
scorePosition' chs emp pos
    -- If there are no empty points to consider, just return what we have
    | Set.null emp = chs
    -- Otherwise, pick an arbitrary point in the empty set and expand it
    | otherwise = scorePosition' (expanded:chs) (emp Set.\\ expanded) pos
  where
    expanded = expandEmptyPoint (Set.singleton (head (Set.toList emp))) pos

expandEmptyPoint :: Chain -> Position -> Chain
expandEmptyPoint ch pos
    | Set.null libs = ch
    | otherwise = expandEmptyPoint (Set.union ch libs) pos
  where
    libs = libertiesOfChain Empty ch pos


prettyPrintPosition :: Position -> String
prettyPrintPosition pos = intercalate "\n" (map stringForRow indices)
  where
    stringForRow x = intersperse ' ' $ map (charForPoint x) indices
    charForPoint x y = case (Set.member pt allBls, Set.member pt allWhts) of
        (True, False) -> '●'
        (False, True) -> '○'
        otherwise -> '+'
      where
        pt = (x, y)
    allBls = allOfColor Black pos
    allWhts = allOfColor White pos
    indices = [1 .. (size pos)]


-- A Ruleset gives the rules of play

data NoPlayReason = InvalidPlay | GameOver
data Ruleset = Ruleset { 
    firstToPlay :: Color,
    positionForPlay :: Color -> Maybe Point -> Game -> Either NoPlayReason Position 
}

instance Show Ruleset where
    show rules = "<ruleset>"


defaultPositionForPlay :: Color -> Maybe Point -> Game -> Either NoPlayReason Position
-- Handle passes
defaultPositionForPlay _ Nothing (Game _ (h1:h2:hs) _)
    | h1 == h2 = Left GameOver
    | otherwise = Right h1
defaultPositionForPlay _ Nothing (Game _ (h:hs) _) = Right h
-- Handle normal plays
defaultPositionForPlay color (Just pt) game
    -- Determine if the point is off the board
    | not $ allowedPoint (size (head (history game))) pt = Left InvalidPlay
    -- Determine if the point to be played is already occupied
    | Set.member pt $ allOccupied (head (history game)) = Left InvalidPlay
    -- Determine if the play violates ko
    | elem newPos (history game) = Left InvalidPlay
    -- Otherwise play the point
    | otherwise = Right newPos
    where
        newPos = removeCaptured $ positionByPlaying color pt (head (history game))
        removeCaptured pos = Set.foldl (\curr ch -> positionByRemoving (opposingColor color) ch curr) pos (capturedBy color pos)

-- The default ruleset's way of determining captures
capturedBy :: Color -> Position -> Set Chain
capturedBy color pos = Set.filter (\ch -> Set.null (libertiesOfChain (opposingColor color) ch pos)) (chainsOfColor (opposingColor color) pos)

-- The default, standard ruleset
defaultRules = Ruleset Black defaultPositionForPlay


-- A Game is a list of past positions possibly the next color to play

data Game = Game { rules :: Ruleset, history :: [Position], toPlay :: Maybe Color } deriving Show

-- Create a new, empty game of the given size
gameWithSize :: Int -> Ruleset -> Game
gameWithSize n rules = Game rules [emptyPosition n] $ Just (firstToPlay rules)

stepGame :: Maybe Point -> Game -> (Bool, Game)
-- Test to see if the game is over
stepGame _ gm@(Game _ _ Nothing) = (False, gm)
-- Otherwise try to play at the point
stepGame pt gm@(Game r h (Just t)) = case (positionForPlay r t pt gm) of
    -- The play was valid and returned a new position
    Right pos -> (True, Game r (pos:h) $ Just (opposingColor t))
    -- The play was invalid
    Left InvalidPlay -> (False, gm)
    -- The play resulted in the end of the game
    Left GameOver -> (True, Game r h Nothing)

latestPosition :: Game -> Position
latestPosition = head . history
