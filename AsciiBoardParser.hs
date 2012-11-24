module AsciiBoardParser where

-- This module parses boards in the standard ASCII format into positions

import Text.ParserCombinators.Parsec
import Data.Array

import GoModel
import Utils

boardP :: Parser [[Player]]
boardP = sepEndBy
    (many
        $ choice [
            char '#' >> return Black,
            char 'O' >> return White,
            char '.' >> return Neither])
    $ char '\n'

parseToNestedList :: String -> Maybe [[Player]]
parseToNestedList str = case parse boardP "" str of
    Left _ -> Nothing
    Right [[]] -> Nothing
    Right points -> return $ if last points == [] then init points else points

convertToBoard :: [[Player]] -> Maybe (Array Point Player)
convertToBoard pss = do
    (rows, cols) <- dimensions pss
    if rows /= cols
        then Nothing
        else return $ array ((1,1), (rows, rows)) [((x, y), pss !! (x - 1 ) !! (y - 1)) | x <- [1..rows], y <- [1..cols]]

parseToBoard :: String -> Maybe (Array Point Player)
parseToBoard str = parseToNestedList str >>= convertToBoard

parseToPosition :: String -> Maybe Position
parseToPosition str = parseToBoard str >>= positionFromBoard
