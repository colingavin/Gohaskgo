module Main where

import Control.Monad
import Text.ParserCombinators.Parsec
import Data.Set (Set)
import qualified Data.Set as Set

import GoModel


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

-- Trys to get and parse a point, returns Nothing if the user passes
getPoint :: IO (Maybe Point)
getPoint = do
    inPt <- getLine
    case parse commaSeparatedPoint "" (inPt ++ "\n") of
        Right result -> do
            return result
        Left _ -> do
            putStrLn "Invalid input. Try again."
            getPoint

printMostRecentPosition :: Game a => a -> IO ()
printMostRecentPosition = putStrLn . prettyPrintPosition . latestPosition

-- Drives the main loop of interaction
interactGame :: AnyGame -> IO ()
-- Is the game over?
interactGame (Left gm) = do 
    (printMostRecentPosition gm)
    putStrLn "Game over."
    let (blackScore, whiteScore) = score gm
    putStrLn $ "Black: " ++ (show blackScore)
    putStrLn $ "White: " ++ (show whiteScore)
    putStrLn $ (show $ winner gm) ++ " won."

-- If the game isn't over, try to get and play a point from the current player
interactGame (Right gm) = do
    printMostRecentPosition gm
    putStrLn $ (show (nextToPlay gm)) ++ " to play. (Return to pass.)"
    pt <- getPoint
    case play gm pt of
        Right ng -> interactGame ng
        Left err -> do
            putStrLn $ "Invalid move: " ++ (show err)
            interactGame (Right gm)

main = interactGame $ Right $ makeNewGame 9
