module Hchess.Game where

import Hchess.Board
import Hchess.Moves
import qualified Data.Map as Map
import Data.Either.Unwrap
import Data.Maybe

data Game = Game Board Team

newGame :: Int -> Int -> [(Team,String)] -> Game
newGame _ _ [] = error "You must have at least two teams"
newGame _ _ [t] = error "You must have at least two teams"
newGame w h teamPlacement = Game (newBoard w h teamPlacement) (fst (head teamPlacement))

newStandardGame :: Game
newStandardGame =
  let
    white = Team North "White"
    black = Team South "Black"
  in
    Game (newStandardBoard white black) white

performMove :: Game -> (Location,Location) -> Maybe Character -> Either String Game
performMove (Game b curTeam) (from,to) promo
  | isLeft fromContents = Left "Invalid location"
  | isNothing fromSquare = Left "The source square is empty"
  | otherwise = Right (Game b curTeam)
  where
    fromContents = pieceAt from b
    fromSquare = fromRight fromContents
    pms = possibleMovesFromLocation b from (length (remainingTeams b)) 
    
