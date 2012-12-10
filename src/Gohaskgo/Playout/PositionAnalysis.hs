module Gohaskgo.Playout.PositionAnalysis where

-- This package contains code that is not part of the model itself,
-- but is used to extract information about game positions.

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Array

import Gohaskgo.Utilities.PointSet (PointSet)
import qualified Gohaskgo.Utilities.PointSet as PS
import Gohaskgo.Model.Base
import Gohaskgo.Model.Point
import Gohaskgo.Model.Chain
import Gohaskgo.Model.Position



-- Finds a point that will capture the given chain, returns an empty set if there
-- is no such chain.
capturePoint :: Chain -> PointSet
capturePoint ch = if (PS.size $ getLiberties ch) == 1 then getLiberties ch else (PS.empty $ PS.width $ getLiberties ch)

-- Determines whether a given position has an empty neighbor
hasLibertyOrFriend :: Point -> Player -> Position -> Bool
--hasLibertyOrFriend p color pos = not $ PS.null $ PS.filter (\neighbor -> (board ! neighbor) `elem` [Neither, color]) (adjacentPoints n p)
hasLibertyOrFriend p color pos = not $ PS.null $ PS.intersection friends (adjacentPoints n p)
  where
    friends = allOfColor color pos
    n = getBoardSize pos

isSelfAtari :: Point -> Player -> Position -> Bool
isSelfAtari p player pos = not $ Set.null $ Set.filter (isSelfAtariForChain p) (chainsWithLiberty player p pos)
  where
    isSelfAtariForChain p ch = PS.size (getLiberties ch) == 2 && not (hasLibertyOrFriend p player pos)

isEye :: Player -> Position -> Point -> Bool
isEye color pos p = (PS.size $ PS.intersection adjs friends) == (PS.size adjs)
  where
    adjs = adjacentPoints n p
    friends = allOfColor color pos
    n = getBoardSize pos

