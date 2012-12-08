module Gohaskgo.Model.Chain where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Array
import qualified Data.Vector.Unboxed as V

import Gohaskgo.Utilities.General
import Gohaskgo.Utilities.PointSet (PointSet)
import qualified Gohaskgo.Utilities.PointSet as PS
import Gohaskgo.Model.Base
import Gohaskgo.Model.Point


-- Chains are contigious sets of points
data Chain = Chain {
    getPoints :: PointSet,
    getLiberties :: PointSet,
    getNeighbors :: PointSet
} deriving (Show, Eq, Ord)

joinChains :: Int -> Set Chain -> Chain
joinChains n chs = Set.foldr joinPair (Chain emp emp emp) chs
  where
    emp = PS.empty n
    joinPair (Chain ps ls ns) (Chain ps' ls' ns') = Chain (PS.union ps ps') (PS.union ls ls') (PS.union ns ns')

surroundingPoints :: Int -> PointSet -> PointSet
surroundingPoints n ps = (PS.foldr (\p curr -> PS.union (adjacentPoints n p) curr) (PS.empty n) ps) PS.\\ ps

libertiesOnBoard :: Int -> PointSet -> Array Point Player -> PointSet
libertiesOnBoard n ps board = PS.filter ((== Neither) . (board !)) (surroundingPoints n ps)

removeChain :: Chain -> Array Point Player -> Array Point Player
removeChain (Chain ps _ _) board = board // map (flip pair Neither) (PS.toList ps)
