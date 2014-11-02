module Hchess.Bots where

import Hchess.Board
import Hchess.Moves
import Hchess.Game
import qualified Data.Map as Map
import Data.Either.Unwrap
import Data.Maybe
import System.Random

firstPossibleMoverBot :: Board -> Team -> [String] -> IO (Maybe ((Location,Location),Maybe Character))
firstPossibleMoverBot b t _ =
  return (if null posMoves then Nothing else Just ((from,to),Nothing))
  where
    posMoves = myPossibleMoves t b
    Move (from,to) _ = head posMoves -- doesn't handle promotion

randomMoverBot :: Board -> Team -> [String] -> IO (Maybe ((Location,Location),Maybe Character))
randomMoverBot b t _ = do
  if null posMoves then return Nothing
  else do
    r <- randomRIO (0,(length posMoves) - 1)
    let
      Move (from,to) _ = posMoves !! r
    return (Just ((from,to),Nothing))
  where
    posMoves = myPossibleMoves t b

