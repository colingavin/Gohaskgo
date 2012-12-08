module Gohaskgo.Playout.Heuristics where

import Control.Monad.State
import Data.Array
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Exts

import Debug.Trace (trace)

import Gohaskgo.Utilities.PointSet (PointSet)
import qualified Gohaskgo.Utilities.PointSet as PS
import Gohaskgo.Playout.PositionAnalysis
import Gohaskgo.Utilities.General
import Gohaskgo.Model.Base
import Gohaskgo.Model.Point
import Gohaskgo.Model.Chain
import Gohaskgo.Model.Gameplay
import Gohaskgo.Model.Position


-- Access point to playout heurisitcs
-- Given a set of open points and a game, groups the moves into good, fair, and bad, to be tried in order
classifyMoves :: PointSet -> IncompleteGame -> [PointSet]
classifyMoves ps gm = map (PS.fromList n) $ groupWith (\p -> scores ! p) $ PS.toList ps
  where
    scores = snd $ runState (combinedHeuristics ps gm) emptyScores
    emptyScores = array ((1, 1), (n, n)) [((x, y), 0) | x <- [1..n], y <- [1..n]]
    n = getSize gm

maxPlayoutLength :: [Int]
maxPlayoutLength = map ((*3) . (^2)) [1..]

-- Tests a game to see if passing now (and thereby ending the game) would result in a win
shouldPassToWin :: IncompleteGame -> Bool
shouldPassToWin gm = (winner $ resign gm) == getToPlay gm

-- Tests a game to determine if the opponent has all-but-won
shouldResign :: IncompleteGame -> Bool
shouldResign gm = opponentsCount - playersCount > (getSize gm)^2 `div` 2 || (Set.size $ getHistory gm) > maxPlayoutLength !! (getSize gm)
  where
    playersCount = PS.size $ allOfColor (getToPlay gm) pos
    opponentsCount = PS.size $ allOfColor (opponent $ getToPlay gm) pos
    pos = latestPosition gm

-- A playout heuristic takes a set of open points and returns those that it considers good and bad
type PlayoutHeuristic = PointSet -> IncompleteGame -> State (Array Point Int) ()

-- All the playout heuristics to try in order, may be weighted later
allPlayoutHeuristics :: [PlayoutHeuristic]
allPlayoutHeuristics = [selfAtariHeuristic, captureHeuristic, linesHeuristic, escapeHeuristic, eyesHeuristic]

-- All the heuristics combined into a state transformer on the array of scores
combinedHeuristics :: PlayoutHeuristic
combinedHeuristics ps gm = foldr (>>) (return ()) $ map (\h -> h ps gm) allPlayoutHeuristics

-- Utility method to adjust the set of scores
adjustScores :: Int -> PointSet -> State (Array Point Int) ()
adjustScores dx ps = do
  board <- get
  let adjustment = [(p, (board ! p) + dx) | p <- PS.toList ps]
  put $ board // adjustment

-- Heuristic to avoid self atari
selfAtariHeuristic :: PlayoutHeuristic
selfAtariHeuristic ps gm = adjustScores (-1) $ PS.filter (\p -> isSelfAtari p player pos) ps
  where
    pos = latestPosition gm
    player = getToPlay gm

-- Heuristic to capture opposing stones
captureHeuristic :: PlayoutHeuristic
captureHeuristic ps gm = adjustScores 1 $ PS.intersection capturePoints ps
  where
    capturePoints = Set.foldr (PS.union . capturePoint) (PS.empty $ PS.width ps) (chainsForPlayer opp pos)
    pos = latestPosition gm
    opp = opponent $ getToPlay gm

-- Heuristic to avoid playing on lines 1 and 2
linesHeuristic :: PlayoutHeuristic
linesHeuristic ps gm = adjustScores (-1) $ PS.filter (isOnBadLine boardSize) ps
  where
    isOnBadLine n (x, y) = x == 1 || y == 1 || x == 2 || y == 2 || x == n || y == n || x == n - 1 || y == n - 1
    boardSize = getSize gm

-- Heuristic to save own chains that are in atari
escapeHeuristic :: PlayoutHeuristic
escapeHeuristic ps gm = adjustScores 1 $ PS.intersection escapePoints ps
  where
    escapePoints = Set.foldr (PS.union . capturePoint) (PS.empty $ PS.width ps) (chainsForPlayer (getToPlay gm) (latestPosition gm))

-- Heuristic to avoid playing in own eyes
eyesHeuristic :: PlayoutHeuristic
eyesHeuristic ps gm = adjustScores (-1) eyes
  where
    eyes = PS.filter (isEye player pos) liberties
    liberties = Set.foldr (PS.union . getLiberties) (PS.empty $ PS.width ps) (chainsForPlayer player pos)
    player = getToPlay gm
    pos = latestPosition gm
