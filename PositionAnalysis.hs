module PositionAnalysis where

-- This package contains code that is not part of the model itself,
-- but is used to extract information about game positions.

import Data.Set (Set)
import qualified Data.Set as Set

import GoModel

-- Finds a point that will capture the given chain, returns an empty set if there
-- is no such chain.
capturePoint :: Chain -> Set Point
capturePoint ch = if (Set.size $ getLiberties ch) == 1 then getLiberties ch else Set.empty
