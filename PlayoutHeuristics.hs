module PlayoutHeuristics where

import Control.Monad.State
import Data.Array
import Data.Set (Set)
import qualified Data.Set as Set
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import GHC.Exts

import Debug.Trace (trace)

import Utils
import GoModel
import PositionAnalysis

-- Access point to playout heurisitcs
-- Given a set of open points and a game, groups the moves into good, fair, and bad, to be tried in order
classifyMoves :: Set Point -> IncompleteGame -> [Set Point]
classifyMoves ps gm = map Set.fromList $ groupWith (\p -> scores ! p) $ Set.toList ps
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
shouldResign gm = opponentsCount - playersCount > (getSize gm)^2 `div` 2 || (length $ (getHistory gm)) > maxPlayoutLength !! (getSize gm)
  where
    playersCount = Set.size $ allOfColor (getToPlay gm) pos
    opponentsCount = Set.size $ allOfColor (opponent $ getToPlay gm) pos
    pos = latestPosition gm

-- A playout heuristic takes a set of open points and returns those that it considers good and bad
type PlayoutHeuristic = Set Point -> IncompleteGame -> State (Array Point Int) ()

-- All the playout heuristics to try in order, may be weighted later
allPlayoutHeuristics :: [PlayoutHeuristic]
allPlayoutHeuristics = [selfAtariHeuristic, captureHeuristic, linesHeuristic, escapeHeuristic, eyesHeuristic]

-- All the heuristics combined into a state transformer on the array of scores
combinedHeuristics :: PlayoutHeuristic
combinedHeuristics ps gm = foldr (>>) (return ()) $ map (\h -> h ps gm) allPlayoutHeuristics

-- Utility method to adjust the set of scores
adjustScores :: Int -> Set Point -> State (Array Point Int) ()
adjustScores dx ps = do
  board <- get
  let adjustment = [(p, (board ! p) + dx) | p <- Set.toList ps]
  put $ board // adjustment

-- Heuristic to avoid self atari
selfAtariHeuristic :: PlayoutHeuristic
selfAtariHeuristic ps gm = adjustScores (-1) $ Set.filter (\p -> isSelfAtari p player pos) ps
  where
    pos = latestPosition gm
    player = getToPlay gm

-- Heuristic to capture opposing stones
captureHeuristic :: PlayoutHeuristic
captureHeuristic ps gm = adjustScores 1 $ Set.intersection capturePoints ps
  where
    capturePoints = flattenSet $ Set.map capturePoint (chainsForPlayer opp pos)
    pos = latestPosition gm
    opp = opponent $ getToPlay gm

-- Heuristic to avoid playing on lines 1 and 2
badPointsForSize :: [Set Point]
badPointsForSize = map badPoints [0..]
  where
    badPoints n = Set.filter (isOnBadLine n) $ Set.fromList [(x, y) | x <- [1..n], y <- [1..n]]
    isOnBadLine n (x, y) = x == 1 || y == 1 || x == 2 || y == 2 || x == n || y == n || x == n - 1 || y == n - 1

linesHeuristic :: PlayoutHeuristic
linesHeuristic ps gm = adjustScores (-1) $ Set.intersection (badPointsForSize !! boardSize) ps
  where
    boardSize = getSize gm

-- Heuristic to save own chains that are in atari
escapeHeuristic :: PlayoutHeuristic
escapeHeuristic ps gm = adjustScores 1 $ Set.intersection escapePoints ps
  where
    escapePoints = flattenSet $ Set.map capturePoint (chainsForPlayer (getToPlay gm) (latestPosition gm))

-- Heuristic to avoid playing in own eyes
eyesHeuristic :: PlayoutHeuristic
eyesHeuristic ps gm = adjustScores (-1) eyes
  where
    eyes = Set.filter (isEye (getToPlay gm) (latestPosition gm)) ps
