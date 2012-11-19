import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Text.ParserCombinators.Parsec
import Data.List

-- Utility

anyElement :: Set a -> Maybe a
anyElement s
    | Set.null s = Nothing
    | otherwise = Just $ head $ Set.elems s


-- Points represent intersections on the board

data Point = Point (Int, Int) deriving (Show, Eq, Ord, Read)

-- Utility to add two points together coordinant-wise
addPoint :: Point -> Point -> Point
addPoint (Point (ax, ay)) (Point (bx, by)) = Point (ax + bx, ay + by)

-- Utility to determine if a point is allowed on a given sized board
allowedPoint :: Int -> Point -> Bool
allowedPoint n (Point (x, y)) = (x > 0) && (y > 0) && (x <= n) && (y <= n)

-- Get all the adjacent points of a given point on the specified sized board
adjacentPoints :: Int -> Point -> Set Point
adjacentPoints n pt = Set.fromList $ filter (allowedPoint n) $ map (addPoint pt) directions
    where directions = [Point (0, 1), Point (1, 0), Point (0, -1), Point (-1, 0)]


-- The colors of the players

data Color = White | Black deriving (Eq, Show)

-- Get the opposing color
opposingColor :: Color -> Color
opposingColor White = Black
opposingColor Black = White


-- Chains are sets of points which share liberties

data Chain = Chain (Set Point) deriving (Show, Ord, Eq)

toList :: Chain -> [Point]
toList (Chain pts) = Set.toList pts

toSet :: Chain -> Set Point
toSet (Chain pts) = pts

joinChains :: Set Chain -> Chain
joinChains = Chain . Set.foldr (\ch a -> Set.union (toSet ch) a) Set.empty

appendToChain :: Point -> Chain -> Chain
appendToChain pt (Chain st) = Chain (Set.insert pt st)

surroundingPoints :: Int -> Chain -> Set Point
surroundingPoints n (Chain pts) = (Set.unions $ map (adjacentPoints n) (Set.toList pts)) Set.\\ pts


-- Positions represent the state of the board

data Position = Position { size :: Int, blackChains :: Set Chain, whiteChains :: Set Chain } deriving (Show, Eq)

-- Create a new, empty position
emptyPosition :: Int -> Position
emptyPosition n = Position n  Set.empty Set.empty

-- Find all the chains of a specific color
chainsOfColor :: Color -> Position -> Set Chain
chainsOfColor White = whiteChains
chainsOfColor Black = blackChains

-- Collect all the points occupied by a given color
allOfColor :: Color -> Position -> Set Point
allOfColor color = toSet . joinChains . chainsOfColor color

-- Collect all the occupied positions
allOccupied :: Position -> Set Point
allOccupied pos = Set.union (allOfColor White pos) (allOfColor Black pos)

-- Find all the liberties of a given chain
libertiesOfChain :: Color -> Chain -> Position -> Set Point
libertiesOfChain color ch pos = (surroundingPoints (size pos) ch) Set.\\ allOfColor (opposingColor color) pos

-- Determine which chains have a point as a liberty
chainsWithLiberty :: Color -> Point -> Position -> Set Chain
chainsWithLiberty color pt pos = Set.filter (\ch -> Set.member pt (libertiesOfChain color ch pos)) (chainsOfColor color pos)

-- Create a new position by playing at a point
positionByPlaying :: Color -> Point -> Position -> Position
positionByPlaying color pt pos
    | not $ allowedPoint (size pos) pt = error "Cannot play that point."
    | color == White = Position (size pos) (blackChains pos) merged
    | color == Black = Position (size pos) merged (whiteChains pos)
    where 
        -- Merge the chains that have pt as a liberty with pt and add them to the rest
        merged = Set.insert (appendToChain pt (joinChains withLiberty)) withoutLiberty
        -- Find the chains that don't have pt as a liberty
        withoutLiberty = (chainsOfColor color pos) Set.\\ withLiberty
        -- Find the chains that have pt as a liberty
        withLiberty = chainsWithLiberty color pt pos

-- Create a new position by removing a given chain
positionByRemoving :: Color -> Chain -> Position -> Position
positionByRemoving White ch (Position n bs ws) = Position n bs (Set.delete ch ws)
positionByRemoving Black ch (Position n bs ws) = Position n (Set.delete ch bs) ws 


prettyPrintPosition :: Position -> String
prettyPrintPosition pos = intercalate "\n" (map stringForRow indices)
  where
    stringForRow x = intersperse ' ' $ map (charForPoint x) indices
    charForPoint x y = case (Set.member pt allBls, Set.member pt allWhts) of
        (True, False) -> '●'
        (False, True) -> '○'
        otherwise -> '+'
      where
        pt = Point (x, y)
    allBls = allOfColor Black pos
    allWhts = allOfColor White pos
    indices = [1 .. (size pos)]


-- A Ruleset gives the rules of play

data NoPlayReason = InvalidPlay | GameOver
data Ruleset = Ruleset { 
    firstToPlay :: Color,
    positionForPlay :: Color -> Maybe Point -> Game -> Either NoPlayReason Position 
}

instance Show Ruleset where
    show rules = "<ruleset>"


defaultPositionForPlay :: Color -> Maybe Point -> Game -> Either NoPlayReason Position
-- Handle passes
defaultPositionForPlay _ Nothing (Game _ (h1:h2:hs) _)
    | h1 == h2 = Left GameOver
    | otherwise = Right h1
defaultPositionForPlay _ Nothing (Game _ (h:hs) _) = Right h
-- Handle normal plays
defaultPositionForPlay color (Just pt) game
    -- Determine if the point is off the board
    | not $ allowedPoint (size (head (history game))) pt = Left InvalidPlay
    -- Determine if the point to be played is already occupied
    | Set.member pt $ allOccupied (head (history game)) = Left InvalidPlay
    -- Determine if the play violates ko
    | elem newPos (history game) = Left InvalidPlay
    -- Otherwise play the point
    | otherwise = Right newPos
    where
        newPos = removeCaptured $ positionByPlaying color pt (head (history game))
        removeCaptured pos = Set.foldl (\curr ch -> positionByRemoving (opposingColor color) ch curr) pos (capturedBy color pos)

-- The default ruleset's way of determining captures
capturedBy :: Color -> Position -> Set Chain
capturedBy color pos = Set.filter (\ch -> Set.null (libertiesOfChain (opposingColor color) ch pos)) (chainsOfColor (opposingColor color) pos)

-- The default, standard ruleset
defaultRules = Ruleset Black defaultPositionForPlay


-- A Game is a list of past positions possibly the next color to play

data Game = Game { rules :: Ruleset, history :: [Position], toPlay :: Maybe Color } deriving Show

-- Create a new, empty game of the given size
gameWithSize :: Int -> Ruleset -> Game
gameWithSize n rules = Game rules [emptyPosition n] $ Just (firstToPlay rules)

stepGame :: Maybe Point -> Game -> (Bool, Game)
-- Test to see if the game is over
stepGame _ gm@(Game _ _ Nothing) = (False, gm)
-- Otherwise try to play at the point
stepGame pt gm@(Game r h (Just t)) = case (positionForPlay r t pt gm) of
    -- The play was valid and returned a new position
    Right pos -> (True, Game r (pos:h) $ Just (opposingColor t))
    -- The play was invalid
    Left InvalidPlay -> (False, gm)
    -- The play resulted in the end of the game
    Left GameOver -> (True, Game r h Nothing)


-- User interaction

actualPair :: GenParser Char st (Maybe Point)
actualPair = do
    x <- (many1 digit)
    spaces
    char ','
    spaces
    y <- (many1 digit)
    return $ Just $ Point ((read x), (read y))

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
