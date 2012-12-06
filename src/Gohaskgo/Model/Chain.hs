module Gohaskgo.Model.Chain where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Array

import Gohaskgo.Utilities.General

import Gohaskgo.Model.Base
import Gohaskgo.Model.Point


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
