module Gohaskgo.Model.Position where

import Data.Array
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List

import Gohaskgo.Utilities.PointSet (PointSet)
import qualified Gohaskgo.Utilities.PointSet as PS
import Gohaskgo.Model.Base
import Gohaskgo.Model.Point
import Gohaskgo.Model.Chain
import Gohaskgo.Model.Zobrist


-- Positions are the main representation of the state of a board
data Position = Position {
    getBoardSize :: Int,
    getBlackChains :: Set Chain,
    getWhiteChains :: Set Chain,
    getZobristData :: ZobristData,
    getHash :: ZobristHash,
    getBlackPoints :: PointSet,
    getWhitePoints :: PointSet,
    getOpenPoints :: PointSet
} deriving (Eq, Show)

-- Create a new, empty position
emptyPosition :: Int -> ZobristData -> Position
emptyPosition n zob = Position n Set.empty Set.empty zob (emptyBoardHash zob) (PS.empty n) (PS.empty n) $ PS.fromList n (boardPoints n)

-- Play at a point with a color (note, no pattern for Neither, this should be a crash)
positionByPlaying :: Player -> Point -> Position -> Either PlayError Position
positionByPlaying color pt pos
    | not $ allowedPoint n pt = Left Other
    | not $ PS.elem pt opens = Left OccupiedPoint
    | color == White = Right $ Position n (updateLiberties Black newOpens bs) merged zob newHash allbs (PS.insert pt allws) newOpens
    | color == Black = Right $ Position n merged (updateLiberties White newOpens ws) zob newHash (PS.insert pt allbs) allws newOpens
  where
    newHash = changePoint pt Neither color zob hash
    newOpens = PS.delete pt opens
    -- Merge the chains that have pt as a liberty with pt and add them to the rest
    merged = Set.insert (insertPoint pt color newOpens n (joinChains n withLiberty)) withoutLiberty
    -- Find the chains that do and don't have pt as a liberty
    (withLiberty, withoutLiberty) = Set.partition (\(Chain _ ls _) -> PS.elem pt ls) (chainsForPlayer color pos)
    -- Get fields from pos
    n = getBoardSize pos
    bs = getBlackChains pos
    ws = getWhiteChains pos
    zob = getZobristData pos
    hash = getHash pos
    allbs = getBlackPoints pos
    allws = getWhitePoints pos
    opens = getOpenPoints pos

-- Insert a point into a chain and update its liberties
insertPoint :: Point -> Player -> PointSet -> Int -> Chain -> Chain
insertPoint p color empties n (Chain ps ls ns)
    | PS.null ps = Chain newPoints newLiberties newNeighbors
    | otherwise = Chain newPoints ((PS.union newLiberties (PS.delete p ls)) PS.\\ ps) ((PS.union newNeighbors (PS.delete p ns)) PS.\\ ps)
  where
    newNeighbors = adjacentPoints n p
    newLiberties = PS.intersection empties newNeighbors
    newPoints = PS.insert p ps

-- Update the liberties of the chains of a given color
updateLiberties :: Player -> PointSet -> Set Chain -> Set Chain
updateLiberties color empties chs = Set.map updateOne chs
  where
    updateOne (Chain ps _ ns) = Chain ps (PS.intersection empties ns) ns

-- Create a new position by clearing all captured chains of a given color
positionByClearing :: Player -> Position -> Position
positionByClearing color pos
    | color == White = Position n (updateLiberties Black newOpens bs) (ws Set.\\ capturedChains) zob newHash allbs (allws PS.\\ capturedPoints) newOpens
    | color == Black = Position n (bs Set.\\ capturedChains) (updateLiberties White newOpens ws) zob newHash (allbs PS.\\ capturedPoints) allws newOpens
    | color == Neither = error "Can't clear empty."
  where
    capturedChains = Set.filter (\ch -> PS.null (getLiberties ch)) (chainsForPlayer color pos)
    joined = joinChains n capturedChains
    capturedPoints = getPoints joined
    newHash = PS.foldr (\pt accum -> changePoint pt color Neither zob hash) hash capturedPoints
    newOpens = PS.union (getOpenPoints pos) capturedPoints
    -- Get fields from pos
    n = getBoardSize pos
    bs = getBlackChains pos
    ws = getWhiteChains pos
    zob = getZobristData pos
    hash = getHash pos
    allbs = getBlackPoints pos
    allws = getWhitePoints pos

-- Construct a position from a board in Array form
positionFromBoard :: Array Point Player -> ZobristData -> Maybe Position
positionFromBoard board zob = playThrough (indices board) (emptyPosition (fst $ snd $ bounds board) zob)
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
chainsWithLiberty color pt pos = Set.filter (\ch -> PS.elem pt (getLiberties ch)) $ chainsForPlayer color pos

-- Find all the unoccupied points
allOfColor :: Player -> Position -> PointSet
allOfColor Black = getBlackPoints
allOfColor White = getWhitePoints
allOfColor Neither = getOpenPoints

-- Calculates the chinese score of the position for (Black, White)
scorePosition :: Position -> (Int, Int)
scorePosition pos = (scoreColor Black, scoreColor White)
  where
    allPts = Set.fromList $ boardPoints n
    emptyChains = emptyConnectedRegions [] (allOfColor Neither pos) pos
    scoreEmptyChains chs color = foldr ((+) . PS.size) 0 $ filter isInTerritory chs
      where
        isInTerritory ch = PS.null $ PS.intersection (allOfColor (opponent color) pos) (surroundingPoints n ch)
    scoreColor color = scoreEmptyChains emptyChains color + PS.size (allOfColor color pos)
    -- Get fields from pos
    n = getBoardSize pos
    bs = getBlackChains pos
    ws = getWhiteChains pos

-- Inner method for score calculation: 
    -- [Chain] is the empty chains so far created 
    -- Set Point is the set of unconsidered empty points
emptyConnectedRegions :: [PointSet] -> PointSet -> Position -> [PointSet]
emptyConnectedRegions chs emp pos
    -- If there are no empty points to consider, just return what we have
    | PS.null emp = chs
    -- Otherwise, pick an arbitrary point in the empty set and expand it
    | otherwise = emptyConnectedRegions (expanded:chs) (emp PS.\\ expanded) pos
  where
    expanded = expandEmptyChain (PS.someSingleton emp) pos

-- Take a chain consisting of empty points and expand it until it has no liberties
expandEmptyChain :: PointSet -> Position -> PointSet
expandEmptyChain ch pos
    | PS.null libs = ch
    | otherwise = expandEmptyChain (PS.union ch libs) pos
  where
    libs = (PS.intersection ch (getOpenPoints pos)) PS.\\ ch

-- Create a nice string representation of the position
prettyPrintPosition :: Position -> String
prettyPrintPosition pos = intercalate "\n" (map stringForRow [1 .. n])
  where
    stringForRow x = intersperse ' ' $ map (charForPoint x) [1 .. n]
    charForPoint x y = 
        let pt = (x, y) in
            case (PS.elem pt whites, PS.elem pt blacks, PS.elem pt empties) of
                (_, True, _) -> '●'
                (True, _, _) -> '○'
                otherwise -> '+'
    -- Get fields from pos
    n = getBoardSize pos
    whites = getWhitePoints pos
    blacks = getBlackPoints pos
    empties = getOpenPoints pos
