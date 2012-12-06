module Main where

import System.Environment
import Data.Random
import Data.Random.Source.DevRandom


import Gohaskgo.Model.Base
import Gohaskgo.Model.Position
import Gohaskgo.Model.Gameplay
import Gohaskgo.Model.Zobrist
import Gohaskgo.Utilities.AsciiBoardParser
import Gohaskgo.Playout.Base

readProblem :: String -> IO (Maybe IncompleteGame)
readProblem filename = do
    problemText <- readFile filename
    mPos <- parseToPosition problemText
    case mPos of
        Nothing -> return $ Nothing
        Just pos -> do
            return $ Just $ makeGameFromPosition pos White

main = do
    args <- getArgs
    game <- if length args > 0
        then do
            problem <- readProblem (args !! 0)
            return $ case problem of
                Nothing -> error "Invalid filename."
                Just gm -> Right gm
        else do
            zob <- sampleFrom DevURandom (newZobristData 9)
            return $ Right $ makeNewGame 9 zob
    let afterRand = randomPlayout game
    a <- sampleFrom DevURandom afterRand
    putStrLn $ prettyPrintPosition $ latestPosition a
