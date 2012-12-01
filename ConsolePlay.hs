module Main where

import Data.Random
import Data.Random.Source.DevRandom
import Control.Monad
import Text.ParserCombinators.Parsec
import Data.Set (Set)
import qualified Data.Set as Set

import GoModel
import UCT


-- User input parsing (using Parsec)

actualPair :: Parser (Maybe Point)
actualPair = do
    x <- (many1 digit)
    spaces
    char ','
    spaces
    y <- (many1 digit)
    return $ Just $ (read x, read y)

onlyReturn :: Parser (Maybe Point)
onlyReturn = (string "\n") >> (return Nothing)

commaSeparatedPoint :: Parser (Maybe Point)
commaSeparatedPoint = actualPair <|> onlyReturn


-- User interaction

type MoveSource = IncompleteGame -> IO (Maybe Point)

-- Trys to get and parse a point, returns Nothing if the user passes
getPoint :: MoveSource
getPoint gm = do
    inPt <- getLine
    case parse commaSeparatedPoint "" (inPt ++ "\n") of
        Right result -> do
            return result
        Left _ -> do
            putStrLn "Invalid input. Try again."
            getPoint gm

getUCTMove :: MoveSource
getUCTMove gm = do
    move <- sampleFrom DevURandom $ uctRespond gm 200 1
    return $ Just move

printMostRecentPosition :: Game a => a -> IO ()
printMostRecentPosition = putStrLn . prettyPrintPosition . latestPosition

playMove :: Maybe Point -> IncompleteGame -> [MoveSource] -> IO ()
playMove (Just pt) gm players = case play gm pt of
    Right ng -> interactGame (Right ng) (drop 1 players)
    Left err -> do
        putStrLn $ "Invalid move: " ++ (show err)
        interactGame (Right gm) players
playMove Nothing gm players = interactGame (pass gm) (drop 1 players)

-- Drives the main loop of interaction
interactGame :: AnyGame -> [MoveSource] -> IO ()
-- Is the game over?
interactGame (Left gm) _ = do 
    (printMostRecentPosition gm)
    putStrLn "Game over."
    let (blackScore, whiteScore) = score gm
    putStrLn $ "Black: " ++ (show blackScore)
    putStrLn $ "White: " ++ (show whiteScore)
    putStrLn $ (show $ winner gm) ++ " won."

-- If the game isn't over, try to get and play a point from the current player
interactGame (Right gm) players = do
    printMostRecentPosition gm
    putStrLn $ (show (getToPlay gm)) ++ " to play."
    move <- (head players) gm
    playMove move gm players

players = cycle [getPoint, getUCTMove]

main = interactGame (Right $ makeNewGame 9) players
