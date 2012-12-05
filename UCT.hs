module UCT where

import Data.List hiding (insert)
import Data.Tree
import Data.Tree.Zipper hiding (last)
import Data.Random
import GHC.Exts
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Debug.Trace (trace)

import GoModel
import GoTypes
import Utils
import Playout

data SearchNode = SearchNode {
    getPlayer :: Player,
    getMove :: Point,
    getWins :: Int,
    getVisits :: Int,
    getAvailableMoves :: Set Point
} deriving Show

prettyShowNode :: SearchNode -> String
prettyShowNode (SearchNode _ m w v _) = "Move: " ++ (show m) ++ ". W/V: " ++ (show w) ++ "/" ++ (show v)

setAvaliableMoves :: Set Point -> SearchNode -> SearchNode
setAvaliableMoves as (SearchNode p m w v _) = (SearchNode p m w v as)

-- Update
updateNode :: Int -> Int -> SearchNode -> SearchNode
updateNode playouts plusWins (SearchNode player m wins visits as) = (SearchNode player m (wins + plusWins) (visits + playouts) as)

type UCT = Tree SearchNode

type UCTZipper = TreePos Full SearchNode

uctRespond :: IncompleteGame -> Int -> Int -> RVar Point
uctRespond gm iters playouts = uctSearch (return rootNode) gm iters playouts
  where
    rootNode = SearchNode (opponent $ getToPlay gm) (0,0) 0 0 (emptyPoints gm)

uctSearch :: UCT -> IncompleteGame -> Int -> Int -> RVar Point
--uctSearch tree _ 0 _ | trace (drawTree (fmap show tree)) False = undefined
uctSearch tree _ 0 _ | trace (intercalate "\n" $ map prettyShowNode $ sortWith getVisits $ head $ tail $ levels tree) False = undefined
-- With no iterations left, return the most visited top level move
uctSearch tree _ 0 _ = return $ getMove $ last $ sortWith getVisits $ head $ tail $ levels tree
-- Perform a recursive Monte Carlo search for a given number of iterations
uctSearch tree gm iters playouts = do
    -- Find the node at the bottom of the tree to expand
    let (toExpand, gameAtBottom) = searchDown tree gm
    -- Randomly expand it using heuristics
    let (newNode, expandedGame) = expandNode toExpand gameAtBottom
    -- Preform the specified number of playouts on the new node
    wins <- playoutNode newNode expandedGame playouts
    -- Propogate the results back up the tree and repeat
    uctSearch (propogateUp newNode playouts wins) gm (iters - 1) playouts

searchDown :: UCT -> IncompleteGame -> (UCTZipper, IncompleteGame)
searchDown tree gm = searchDown' (fromTree tree) gm

searchDown' :: UCTZipper -> IncompleteGame -> (UCTZipper, IncompleteGame)
searchDown' zipper gm
    -- While the current node has children and has been explored, traverse downward, directed by the UCB
    | hasChildren zipper && (Set.null $ getAvailableMoves $ label zipper) = let selected = uctSelectChild zipper in
        searchDown' selected (fromRight $ play gm $ getMove $ label selected)
    | otherwise = (zipper, gm)

-- Use UCB to select a child of the given node.
-- Pre-condition: the input zipper has children
uctSelectChild :: UCTZipper -> UCTZipper
uctSelectChild zipper = selectChild' (-1) undefined (firstChild zipper)
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
expandNode :: UCTZipper -> IncompleteGame -> (UCTZipper, IncompleteGame)
expandNode zipper gm = case firstAvaliableMove (Set.toList $ getAvailableMoves node) of
    -- If there was no move to make, set avaliable moves to {}
    Nothing ->
        let node' = setAvaliableMoves Set.empty node
            zipper' = setLabel node' zipper
        in (zipper', gm)
    -- Otherwise, create the new search node with that move and append it to the bottom of the tree
    Just (ng, pt) ->
        let newChild = SearchNode (opponent $ getPlayer node) pt 0 0 (emptyPoints ng)
            node' = setAvaliableMoves (Set.delete pt $ getAvailableMoves node) node
            zipper' = setLabel node' zipper
            zipper'' = insert (return newChild) $ children zipper'
        in (zipper'', ng)
  where
    node = label zipper
    firstAvaliableMove [] = Nothing
    firstAvaliableMove (x:xs) = case play gm x of
        Right ng -> Just (ng, x)
        Left _ -> firstAvaliableMove xs

-- Playout a given node some number of times, returning the number of wins
playoutNode :: UCTZipper -> IncompleteGame -> Int -> RVar Int
playoutNode zipper gm playouts = do
    let node = label zipper
    winners <- sequence $ replicate playouts $ randomWinner (Right gm)
    return $ count (== (getPlayer node)) winners

-- Propogate the playout results from the given node back up the tree
-- First Int argument is number of playouts, second is number of wins
propogateUp :: UCTZipper -> Int -> Int -> UCT
--propogateUp zipper playouts wins | trace "propogateUp" False = undefined
propogateUp zipper playouts wins
    | isRoot zipper = toTree $ setLabel updated zipper
    | otherwise = propogateUp (fromJust $ parent $ setLabel updated zipper) playouts (playouts - wins)
  where
    updated = updateNode playouts wins node
    node = label zipper
