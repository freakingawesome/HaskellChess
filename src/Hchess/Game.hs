module Hchess.Game where

import Hchess.Board
import Hchess.Moves
import qualified Data.Map as Map
import Data.Either.Unwrap
import Data.Maybe

data Game = Game Board [Team] deriving (Eq,Show)

newGame :: Int -> Int -> [(Team,String)] -> Game
newGame _ _ [] = error "You must have at least two teams"
newGame _ _ [t] = error "You must have at least two teams"
newGame w h teamPlacement = Game (newBoard w h teamPlacement) (getTurns (map (\(t,s) -> t) teamPlacement))

newStandardGame :: Game
newStandardGame =
  let
    white = Team North "White"
    black = Team South "Black"
  in
    Game (newStandardBoard white black) (getTurns [white,black])

performMove :: Game -> (Location,Location) -> Maybe Character -> Either String Game
performMove (Game b teams) (from,to) promo
  | isLeft fromContents = Left "Invalid location"
  | isNothing fromSquare = Left "The source square is empty"
  | fromTeam /= currentTeam (Game b teams) = Left $ "It is not " ++ (teamName fromTeam) ++ "'s turn"
  | isLeft pms = Left "Illegal move" 
  | null possibleTargetLocs = Left "This piece is currently incapacitated"
  | not (to `elem` possibleTargetLocs) = Left "Illegal move"
  | to `elem` possibleTargetLocs = Right commitMove
  | otherwise = Left "NOT YET IMPLEMENTED" -- causes inf loop Right (Game b teams)
  where
    fromContents = pieceAt from b
    fromSquare = fromRight fromContents
    pms = possibleMovesFromLocation b from (length (remainingTeams b)) 
    possibleTargetLocs = map (\(Move (_,to') _) -> to') (fromRight pms)
    fromTeam = getTeam (fromJust fromSquare)
    targetMoves = [ Move (from',to') b |  Move (from',to') b <- fromRight pms, to' == to ]
    singleMove =
      if length targetMoves == 1 then
        head targetMoves
      else
        error "Promotion not yet implemented"
    commitMove = Game (getBoardFromMove singleMove) (tail teams)
    
getTurns :: Ord x => [x] -> [x]
getTurns [] = []
getTurns x = x ++ (getTurns x)

turnsToTeams :: Ord x => [x] -> [x]
turnsToTeams [] = []
turnsToTeams (x:xs) = x : turnsToTeams' xs
  where 
    turnsToTeams' (x':xs')
      | x' == x = []
      | otherwise = x' : turnsToTeams' xs' 

teamCount :: Ord x => [x] -> Int
teamCount x = length (turnsToTeams x)

removeTeam :: Ord x => [x] -> x -> [x]
removeTeam [] _ = []
removeTeam (x:xs) r
  | x == r && head xs == r = []
  | x == r = removeTeam xs r
  | otherwise = x : removeTeam xs r

currentTeam :: Game -> Team
currentTeam (Game _ []) = error "No teams defined"
currentTeam (Game _ (cur:_)) = cur

