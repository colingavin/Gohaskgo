module PlayoutHeuristics where

import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)

import Utils
import GoModel

-- Access point to playout heurisitcs
-- Given a set of open points and a game, groups the moves into good, fair, and bad, to be tried in order
classifyMoves :: Set Point -> IncompleteGame -> (Set Point, Set Point, Set Point)
classifyMoves ps gm = (good, ps Set.\\ (Set.union good bad), bad)
  where
    (good, bad) = foldr (\(g, b) (cg, cb) -> (Set.union g cg, Set.union b cb) ) (Set.empty, Set.empty) classifications
    classifications = map (\h -> h ps gm) allPlayoutHeuristics

maxPlayoutLength :: [Int]
maxPlayoutLength = map ((*3) . (^2)) [1..]

-- Tests a game to determine if the opponent has all-but-won
shouldResign :: IncompleteGame -> Bool
shouldResign gm = opponentsCount - playersCount > (getSize gm)^2 `div` 2 || (length $ (getHistory gm)) > maxPlayoutLength !! (getSize gm)
  where
    playersCount = Set.size $ allOfColor (getToPlay gm) pos
    opponentsCount = Set.size $ allOfColor (opponent $ getToPlay gm) pos
    pos = latestPosition gm

-- A playout heuristic takes a set of open points and returns those that it considers good and bad
type PlayoutHeuristic = Set Point -> IncompleteGame -> (Set Point, Set Point)

-- All the playout heuristics to try in order, may be weighted later
allPlayoutHeuristics :: [PlayoutHeuristic]
allPlayoutHeuristics = [selfAtariHeuristic, captureHeuristic, linesHeuristic]

-- Heuristic to avoid self atari
selfAtariHeuristic :: PlayoutHeuristic
selfAtariHeuristic ps gm = (Set.empty, Set.filter isSelfAtari ps)
  where
    isSelfAtari p = not $ Set.null $ Set.filter (\ch -> Set.size (getLiberties ch) == 2) (chainsWithLiberty player p pos)
    pos = latestPosition gm
    player = getToPlay gm

-- Heuristic to capture opposing stones
captureHeuristic :: PlayoutHeuristic
captureHeuristic ps gm = (Set.filter isCapture ps, Set.empty)
  where
    isCapture p = not $ Set.null $ Set.filter (\ch -> Set.size (getLiberties ch) == 1) (chainsWithLiberty opp p pos)
    pos = latestPosition gm
    opp = opponent $ getToPlay gm

-- Heuristic to avoid playing on lines 1 and 2
linesHeuristic :: PlayoutHeuristic
linesHeuristic ps gm = (Set.empty, Set.filter isOnBadLine ps)
  where
    isOnBadLine (x, y) = x == 1 || y == 1 || x == 2 || y == 2 || x == boardSize || y == boardSize || x == boardSize - 1 || y == boardSize - 1
    boardSize = getSize gm

