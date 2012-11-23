module Main where

import Control.Monad
import Data.Random
import Data.Random.Source.DevRandom
import System.Random
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)

import GoModel
import PlayoutHeuristics
import Utils


makeRandomMoveFrom :: [Point] -> IncompleteGame -> RVar (Maybe AnyGame)
makeRandomMoveFrom [] gm = return $ Nothing
makeRandomMoveFrom ps gm = do
    idx <- uniform 0 (length ps - 1)
    case play gm (ps !! idx) of
        Right ng -> return $ Just $ Right ng
        Left _ -> makeRandomMoveFrom (dropIndex ps idx) gm

makeRandomMove :: IncompleteGame -> RVar AnyGame
--makeRandomMove gm | (trace ((prettyPrintPosition $ latestPosition gm) ++ "\n") False) = undefined
makeRandomMove gm | shouldResign gm = return $ Left $ resign gm
makeRandomMove gm = do
    goodMove <- makeRandomMoveFrom (Set.toList good) gm
    fairMove <- makeRandomMoveFrom (Set.toList fair) gm
    badMove <- makeRandomMoveFrom (Set.toList bad) gm
    return $ case chooseFirst [goodMove, fairMove, badMove] of
        Just gm -> gm
        Nothing -> pass gm
  where
    (good, fair, bad) = classifyMoves (emptyPoints gm) gm

randomPlayout :: AnyGame -> RVar FinishedGame
randomPlayout (Left f) = return f
randomPlayout (Right gm) = makeRandomMove gm >>= randomPlayout

randomWinner :: AnyGame -> RVar Player
randomWinner gm = randomPlayout gm >>= (return . winner)

main = do

    let emptyGame = Right $ makeNewGame 9
    let afterRand = randomPlayout emptyGame
    let randWin = randomWinner emptyGame
    putStrLn "Random game:"
    a <- sampleFrom DevURandom afterRand
    putStrLn $ prettyPrintPosition $ latestPosition a
