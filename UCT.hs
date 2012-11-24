module UCT where

import Data.Tree
import Data.Tree.Zipper hiding (last)
import Data.Random
import GHC.Exts
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe

import GoModel
import Utils
import Playout

data SearchNode = SearchNode {
    getNodeToPlay :: Player,
    getMove :: Point,
    getWins :: Int,
    getVisits :: Int,
    getAvailableMoves :: Set Point
} deriving Show

setAvaliableMoves :: Set Point -> SearchNode -> SearchNode
setAvaliableMoves as (SearchNode p m w v _) = (SearchNode p m w v as)

-- Update
updateNode :: Int -> Int -> Player -> SearchNode -> SearchNode
updateNode playouts plusWins fromPlayer (SearchNode player m wins visits as) = (SearchNode player m newWins (visits + playouts) as)
  where newWins = if player == fromPlayer then wins + plusWins else wins

type UCT = Tree SearchNode

type UCTZipper = TreePos Full SearchNode

uctSearch :: UCT -> IncompleteGame -> Int -> Int -> RVar Point
-- With no iterations left, return the most visited top level move
uctSearch tree _ 0 _ = return $ getMove $ last $ sortWith getVisits $ head $ levels tree
-- Perform a recursive Monte Carlo search for a given number of iterations
uctSearch tree gm iters playouts = do
    -- Find the node at the bottom of the tree to expand
    let (toExpand, gameAtBottom) = searchDown tree gm
    -- Randomly expand it using heuristics
    (newNode, expandedGame) <- expandNode toExpand gameAtBottom
    -- Preform the specified number of playouts on the new node
    wins <- playoutNode newNode expandedGame playouts
    -- Propogate the results back up the tree and repeat
    uctSearch (propogateUp newNode playouts wins) gm (iters - 1) playouts

searchDown :: UCT -> IncompleteGame -> (UCTZipper, IncompleteGame)
searchDown tree gm = searchDown' (fromTree tree) gm

searchDown' :: UCTZipper -> IncompleteGame -> (UCTZipper, IncompleteGame)
searchDown' zipper gm
    -- While the current node has children and avaliable moves, traverse downward, directed by the UCB
    | hasChildren zipper && (not $ Set.null $ getAvailableMoves $ label zipper) = let selected = uctSelectChild zipper in
        searchDown' selected (fromRight $ play gm $ getMove $ label selected)
    | otherwise = (zipper, gm)

-- Use UCB to select a child of the given node.
-- Pre-condition: the input zipper has children
uctSelectChild :: UCTZipper -> UCTZipper
uctSelectChild zipper = selectChild' 0 undefined (firstChild zipper)
  where
    -- Inner method to traverse the list of children and find the one with the best UCB
    selectChild' _ bestSoFar Nothing = bestSoFar
    selectChild' bestScore bestSoFar (Just child) = if score > bestScore
        then selectChild' score child (next child)
        else selectChild' bestScore bestSoFar (next child)
      where
        score = uctScore (label child) (getVisits $ label zipper)

-- Calculate the UCB of a given search node, second argument is # of visits to parent node
uctScore :: SearchNode -> Int -> Float
uctScore node parentVisits = wins / visits + sqrt(2 * log(fromIntegral parentVisits) / visits)
  where
    wins = fromIntegral $ getWins node
    visits = fromIntegral $ getVisits node

-- Add a node at the bottom of the tree by playing a random move, directed by playout heuristics
expandNode :: UCTZipper -> IncompleteGame -> RVar (UCTZipper, IncompleteGame)
expandNode zipper gm = do
    let node = label zipper
    -- Get the (new game, move) from the heuristics
    move <- makeRandomMoveFrom (Set.toList $ getAvailableMoves node) gm
    case move of
        -- If there was no move to make, set avaliable moves to {}
        Nothing -> do
            let node' = setAvaliableMoves Set.empty node
            let zipper' = setLabel node' zipper
            return (zipper', gm)
        -- Otherwise, create the new search node with that move and append it to the bottom of the tree
        Just (ng, pt) -> do
            let newChild = SearchNode (opponent $ getNodeToPlay node) pt 0 0 (emptyPoints ng)
            let node' = setAvaliableMoves (Set.delete pt $ getAvailableMoves node) node
            let zipper' = setLabel node' zipper
            let zipper'' = insert (return newChild) $ children zipper'
            return (zipper'', ng)

-- Playout a given node some number of times, returning the number of wins
playoutNode :: UCTZipper -> IncompleteGame -> Int -> RVar Int
playoutNode zipper gm playouts = do
    let node = label zipper
    let moves = Set.toList $ getAvailableMoves node
    winners <- randomWinner (Right gm) >>= (return . replicate playouts)
    return $ count (== (getNodeToPlay node)) winners

-- Propogate the playout results from the given node back up the tree
-- First Int argument is number of playouts, second is number of wins
propogateUp :: UCTZipper -> Int -> Int -> UCT
propogateUp zipper playouts wins
    | isRoot zipper = toTree zipper
    | otherwise = propogateUp (fromJust $ parent $ setLabel updated zipper) playouts wins
  where
    updated = updateNode playouts wins (getNodeToPlay node) node
    node = label zipper
