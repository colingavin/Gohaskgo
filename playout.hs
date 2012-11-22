module Main where

import Control.Monad
import Data.Random
import Data.Random.Source.DevRandom
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)

import GoModel
import Utils


makeRandomMove :: IncompleteGame -> RVar AnyGame
makeRandomMove gm = makeRandomMove' (Set.toList $ emptyPoints gm)
  where
    makeRandomMove' [] = return $ pass gm
    makeRandomMove' ps = do
        idx <- uniform 0 (length ps - 1)
        case play gm (ps !! idx) of
            Right ng -> return $ Right ng
            Left _ -> makeRandomMove' (dropIndex ps idx)

randomPlayout :: AnyGame -> RVar FinishedGame
randomPlayout (Left f) = return f
randomPlayout (Right gm) = makeRandomMove gm >>= randomPlayout

randomWinner :: AnyGame -> RVar Player
randomWinner gm = randomPlayout gm >>= (return . winner)

main = do
    let emptyGame = Right $ makeNewGame 3
    let afterRand = randomPlayout emptyGame
    let randWin = randomWinner emptyGame
    putStrLn "Random game:"
    a <- sampleFrom DevURandom afterRand
    putStrLn $ prettyPrintPosition $ latestPosition a
    putStrLn "Random winner:"
    b <- sampleFrom DevURandom randWin
    putStrLn (show b)
