module Gohaskgo.Playout.Base where

import Control.Monad
import Data.Random
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)

import Gohaskgo.Utilities.PointSet (PointSet)
import qualified Gohaskgo.Utilities.PointSet as PS
import Gohaskgo.Playout.Heuristics
import Gohaskgo.Utilities.General
import Gohaskgo.Model.Base
import Gohaskgo.Model.Point
import Gohaskgo.Model.Gameplay
import Gohaskgo.Model.Position


makeRandomMoveFrom :: [Point] -> IncompleteGame -> RVar (Maybe (IncompleteGame, Point))
makeRandomMoveFrom [] gm = return $ Nothing
makeRandomMoveFrom ps gm = do
    idx <- uniform 0 (length ps - 1)
    let pt = ps !! idx
    case play gm pt of
        Right ng -> return $ Just (ng, pt)
        Left _ -> makeRandomMoveFrom (dropIndex ps idx) gm

makeRandomMove :: IncompleteGame -> RVar AnyGame
--makeRandomMove gm | (trace ((prettyPrintPosition $ latestPosition gm) ++ "\n") False) = undefined
makeRandomMove gm | getLastWasPass gm && shouldPassToWin gm = return $ pass gm
makeRandomMove gm | shouldResign gm = return $ Left $ resign gm
makeRandomMove gm = do
    plays <- mapM (flip makeRandomMoveFrom gm) $ reverse $ classifyMoves (emptyPoints gm) gm
    return $ case msum plays of
        Just (gm, _) -> Right gm
        Nothing -> pass gm

randomPlayout :: AnyGame -> RVar FinishedGame
--randomPlayout (Left f) | trace (prettyPrintPosition $ latestPosition f) False = undefined
randomPlayout (Left f) = return f
randomPlayout (Right gm) = makeRandomMove gm >>= randomPlayout

randomWinner :: AnyGame -> RVar Player
randomWinner gm = randomPlayout gm >>= (return . winner)
