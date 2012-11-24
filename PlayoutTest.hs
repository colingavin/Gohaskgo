module Main where

import Data.Random
import Data.Random.Source.DevRandom

import Playout
import GoModel

main = do
    let emptyGame = Right $ makeNewGame 9
    let afterRand = randomPlayout emptyGame
    let randWin = randomWinner emptyGame
    a <- sampleFrom DevURandom afterRand
    putStrLn $ prettyPrintPosition $ latestPosition a
