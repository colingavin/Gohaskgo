module Main where

import System.Environment
import Data.Random
import Data.Maybe
import System.Random.MWC (create)
import Data.Random.Source.DevRandom

import GoModel
import GoTypes
import AsciiBoardParser
import UCT
import Zobrist

main = do
    mwc <- create
    args <- getArgs
    problemText <- readFile $ args !! 0
    mPos <- parseToPosition problemText
    case mPos of
        Nothing -> putStrLn "Invalid problem file."
        Just pos -> do
            putStrLn "Evaluating:"
            putStrLn $ prettyPrintPosition pos
            let gm = makeGameFromPosition pos Black
            response <- sampleFrom mwc $ uctRespond gm 1000 1
            putStrLn $ "Will respond by playing at: " ++ (show response)
