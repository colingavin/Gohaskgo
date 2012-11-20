module Main where

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
onlyReturn = (string "") >> (return Nothing)

commaSeparatedPoint :: Parser (Maybe Point)
commaSeparatedPoint = actualPair <|> onlyReturn


-- User interaction

-- Trys to get and parse a point, returns Nothing if the user passes
getPoint :: IO (Maybe Point)
getPoint = do
    inPt <- getLine
    case parse commaSeparatedPoint "" inPt of
        Right result -> return result
        Left _ -> do
            putStrLn "Invalid input. Try again."
            getPoint

printMostRecentPosition :: Game -> IO ()
printMostRecentPosition = putStrLn . prettyPrintPosition . latestPosition

-- Drives the main loop of interaction
interactGame :: Game -> IO ()
-- Is the game over?
interactGame gm@(Game _ _ Nothing) = do 
    (printMostRecentPosition gm)
    putStrLn "Game over."
    let (blackScore, whiteScore) = scorePosition (latestPosition gm)
    putStrLn $ "Black: " ++ (show blackScore)
    putStrLn $ "White: " ++ (show whiteScore)

-- If the game isn't over, try to get and play a point from the current player
interactGame gm@(Game _ _ (Just color)) = do
    printMostRecentPosition gm
    putStrLn $ (show color) ++ " to play. (Return to pass.)"
    pt <- getPoint
    case stepGame pt gm of
        (True, ng) -> interactGame ng
        (False, _) -> do
            putStrLn "Invalid move."
            interactGame gm

main = interactGame $ gameWithSize 9 defaultRules
