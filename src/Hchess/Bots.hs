module Hchess.Bots where

import Hchess.Board
import Hchess.Moves
import Hchess.Game
import qualified Data.Map as Map
import Data.Either.Unwrap
import Data.Maybe

-- class ChessBot bot where
-- botMove :: bot -> Game -> Team -> Move

firstPossibleMoverBot :: Board -> Team -> [String] -> IO (Maybe ((Location,Location),Maybe Character))
firstPossibleMoverBot b t _ =
  return (if null posMoves then Nothing else Just ((from,to),Nothing)) -- doesn't handle promotion
  where
    posMoves = myPossibleMoves t b
    Move (from,to) _ = head posMoves

