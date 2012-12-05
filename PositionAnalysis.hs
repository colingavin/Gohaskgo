module PositionAnalysis where

-- This package contains code that is not part of the model itself,
-- but is used to extract information about game positions.

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Array

import GoModel
import GoTypes

-- Finds a point that will capture the given chain, returns an empty set if there
-- is no such chain.
capturePoint :: Chain -> Set Point
capturePoint ch = if (Set.size $ getLiberties ch) == 1 then getLiberties ch else Set.empty

-- Determines whether a given position has an empty neighbor
hasLibertyOrFriend :: Point -> Player -> Position -> Bool
hasLibertyOrFriend p color (Position n board _ _ _ _) = not $ Set.null $ Set.filter (\neighbor -> (board ! neighbor) `elem` [Neither, color]) (adjacentPoints n p)

isSelfAtari :: Point -> Player -> Position -> Bool
isSelfAtari p player pos = not $ Set.null $ Set.filter (isSelfAtariForChain p) (chainsWithLiberty player p pos)
  where
    isSelfAtariForChain p ch = Set.size (getLiberties ch) == 2 && not (hasLibertyOrFriend p player pos)

isEye :: Player -> Position -> Point -> Bool
isEye color (Position n board _ _ _ _) p = Set.null $ Set.filter ((/= color) . (board !)) (adjacentPoints n p)
