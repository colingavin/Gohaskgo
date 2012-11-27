module Main where

import System.Environment
import Data.Random
import Data.Random.Source.DevRandom

import Playout
import GoModel
import AsciiBoardParser

readProblem :: String -> IO (Maybe IncompleteGame)
readProblem filename = do
    problemText <- readFile filename
    case parseToPosition problemText of
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
        else return $ Right $ makeNewGame 9
    let afterRand = randomPlayout game
    a <- sampleFrom DevURandom afterRand
    putStrLn $ prettyPrintPosition $ latestPosition a
