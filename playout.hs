module Main where

import Data.Random
import System.Random.MWC (create)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace)

import GoModel
import Utils


makeRandomMove :: PlayableGame a => a -> RVar AnyGame
makeRandomMove gm = makeRandomMove' (Set.toList $ emptyPoints gm)
  where
    makeRandomMove' [] = return $ case play gm Nothing of Right g -> g
    makeRandomMove' ps = do
        idx <- uniform 0 (length ps - 1)
        case play gm $ Just (ps !! idx) of
            Right ng -> return ng
            Left _ -> makeRandomMove' (dropIndex ps idx)

randomPlayout :: AnyGame -> RVar AnyGame
randomPlayout f@(Left _) = return f
randomPlayout (Right gm) = makeRandomMove gm >>= randomPlayout

main = do
    mwc <- create
    let afterRand = randomPlayout $ Right $ makeNewGame 3
    a <- sampleFrom mwc afterRand
    putStrLn $ prettyPrintPosition $ latestPosition a
