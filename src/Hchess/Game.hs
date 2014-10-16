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
newGame w h teamPlacement = Game (newBoard w h teamPlacement) (getTurns (map fst teamPlacement))

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
  | fromTeam /= currentTeam (Game b teams) = Left $ "It is not " ++ teamName fromTeam ++ "'s turn"
  | isLeft pms = Left "Illegal move" 
  | null possibleTargetLocs = Left "This piece is currently incapacitated"
  | to `notElem` possibleTargetLocs = Left "Illegal move"
  | length targetMoves == 1 =
    if isJust promo then
      Left "Promotion is not valid here"
    else
      Right (commitMove (head targetMoves))
  | otherwise =
    if isNothing promo then
      Left "You must specify a character for promotion"
    else
      if length promoTarget == 1 then
        Right (commitMove (head promoTarget))
      else
        Left "Invalid character for promotion"
  where
    fromContents = pieceAt from b
    fromSquare = fromRight fromContents
    pms = possibleMovesFromLocation b from (length (remainingTeams b)) 
    possibleTargetLocs = map (\(Move (_,to') _) -> to') (fromRight pms)
    fromTeam = getTeam (fromJust fromSquare)
    targetMoves = [ Move (from',to') b | Move (from',to') b <- fromRight pms, to' == to ]
    commitMove mv = Game (getBoardFromMove mv) (tail teams)
    promoTarget = [ Move (from',to') b | Move (from',to') b <- fromRight pms,
      getCharacter (fromJust (fromRight (pieceAt to' b))) == fromJust promo ]
    
isStalemate :: Game -> Bool
isStalemate (Game b (t:_)) = null (myPossibleMoves t b) && not (isKingInCheck t b 1)
isStalemate _ = error "No teams specified"

isCheckmate :: Game -> Bool
isCheckmate (Game b (t:_)) = null (myPossibleMoves t b) && isKingInCheck t b 1
isCheckmate _ = error "No teams specified"

getTurns :: Ord x => [x] -> [x]
getTurns [] = []
getTurns x = x ++ getTurns x

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

