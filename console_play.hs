module Main where

import Text.ParserCombinators.Parsec

import GoModel


actualPair :: GenParser Char st (Maybe Point)
actualPair = do
    x <- (many1 digit)
    spaces
    char ','
    spaces
    y <- (many1 digit)
    return $ Just $ (read x, read y)

onlyReturn :: GenParser Char st (Maybe Point)
onlyReturn = (string "") >> (return Nothing)

commaSeparatedPoint :: GenParser Char st (Maybe Point)
commaSeparatedPoint = actualPair <|> onlyReturn

getPoint :: IO (Maybe Point)
getPoint = do
    inPt <- getLine
    case parse commaSeparatedPoint "" inPt of
        Right result -> return result
        Left _ -> do
            putStrLn "Invalid input. Try again."
            getPoint

printMostRecentPosition :: Game -> IO ()
printMostRecentPosition = putStrLn . prettyPrintPosition . head . history

interactGame :: Game -> IO ()
-- Is the game over?
interactGame gm@(Game _ _ Nothing) = (printMostRecentPosition gm) >> putStrLn "Game over."
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
