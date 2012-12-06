{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}

module Gohaskgo.Model.Gameplay where

import Data.Array
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Error
import Debug.Trace (trace)

import Gohaskgo.Model.Base
import Gohaskgo.Model.Point
import Gohaskgo.Model.Chain
import Gohaskgo.Model.Position
import Gohaskgo.Model.Zobrist

-- There are two different states that a game can be in, these are represented by different types
type AnyGame = Either FinishedGame IncompleteGame

-- IncompleteGames retain their history and other information, they also can be played
data IncompleteGame = IncompleteGame {
    getLatestPosition :: Position,
    getHistory :: Set ZobristHash,
    getToPlay :: Player,
    getLastWasPass :: Bool
} deriving (Show)

makeNewGame :: Int -> ZobristData -> IncompleteGame
makeNewGame n zob = IncompleteGame (emptyPosition n zob) Set.empty Black False

makeGameFromPosition :: Position -> Player -> IncompleteGame
makeGameFromPosition pos toPlay = IncompleteGame pos Set.empty toPlay False

play :: IncompleteGame -> Point -> Either PlayError IncompleteGame
play (IncompleteGame pos hist color _) pt = do
    newPos <- positionByPlaying color pt pos
    let cleared = positionByClearing (opponent color) newPos
    let selfCapture = any (Set.null . getLiberties) $ Set.toList (chainsForPlayer color cleared)
    if selfCapture
        then throwError Suicide
    else if Set.member (getHash cleared) hist
        then throwError KoViolation
    else return $ IncompleteGame cleared (Set.insert (getHash cleared) hist) (opponent color) False

pass :: IncompleteGame -> AnyGame
pass (IncompleteGame pos hist color p) = do
    if p
        then Left $ FinishedGame pos
        else return $ IncompleteGame pos hist (opponent color) True

resign :: IncompleteGame -> FinishedGame
resign (IncompleteGame pos _ _ _) = FinishedGame pos

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

-- Class representing what can be done with any kind of game
class Show a => Game a where
    latestPosition :: a -> Position
    getSize :: a -> Int
    getSize = getBoardSize . latestPosition

instance Game IncompleteGame where
    latestPosition = getLatestPosition

instance Game FinishedGame where
    latestPosition = getLastPosition

-- Make AnyGame an instance for convenience
instance Game AnyGame where
    latestPosition (Left f) = latestPosition f
    latestPosition (Right i) = latestPosition i
