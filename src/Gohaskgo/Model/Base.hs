module Gohaskgo.Model.Base where

import Data.Array
import Control.Monad.Error

-- Go is played by two players, Black and White, some intersections on the board may be Neither
data Player = Black | White | Neither deriving (Eq, Show, Ord, Enum, Ix)

-- Get the opposing Player
opponent :: Player -> Player
opponent White = Black
opponent Black = White
opponent Neither = Neither


-- PlayErrors happen when a point cannot be played at
data PlayError = OccupiedPoint | KoViolation | Suicide | Other deriving (Show)

instance Error PlayError where
    noMsg = Other
