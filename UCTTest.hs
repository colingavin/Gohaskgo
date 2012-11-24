module Main where

import Data.Random
import Data.Maybe
import System.Random.MWC (create)
import Data.Random.Source.DevRandom

import GoModel
import AsciiBoardParser
import UCT

main = do
    putStrLn "Enter a problem:"
    filename <- getLine
    problemText <- readFile filename
    case parseToPosition problemText of
        Nothing -> putStrLn "Invalid"
        Just pos -> do
            putStrLn $ prettyPrintPosition pos
            putStrLn "How many iterations?"
            iters <- getLine
            putStrLn "How many playouts per iteration?"
            playouts <- getLine
            mwc <- create
            let gm = makeGameFromPosition pos Black
            response <- sampleFrom mwc $ uctRespond gm (read iters) (read playouts)
            putStrLn (show response)
