module Hchess.Game where

import Hchess.Board
import Hchess.Moves
import qualified Data.Map as Map
import Data.Either.Unwrap
import Data.Maybe
import Data.List(intercalate)

data Game = Game Board [Team] HalfMoveClock FullMoveNumber deriving (Eq,Show)

type Mover = (Board -> Team -> [String] -> IO (Maybe ((Location,Location),Maybe Character)))
type HalfMoveClock = Int
type FullMoveNumber = Int

newGame :: Int -> Int -> [(Team,String)] -> Game
newGame _ _ [] = error "You must have at least two teams"
newGame _ _ [_] = error "You must have at least two teams"
newGame w h teamPlacement =
  Game (newBoard w h teamPlacement) (getTurns (map fst teamPlacement)) 0 1

white :: Team
white = Team North "White"

black :: Team
black = Team South "Black"

newStandardGame :: Game
newStandardGame =
  Game (newStandardBoard white black) (getTurns [white,black]) 0 1

performMove :: Game -> (Location,Location) -> Maybe Character -> Either String Game
performMove g@(Game b teams hmc fmn) (from,to) promo
  | isLeft fromContents = Left "Invalid location"
  | isNothing fromSquare = Left "The source square is empty"
  | fromTeam /= currentTeam g = Left $ "It is not " ++ teamName fromTeam ++ "'s turn"
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
    toSquare = fromRight (pieceAt to b)
    pms = possibleMovesFromLocation b from (length (remainingTeams b))
    possibleTargetLocs = map (\(Move (_,to') _) -> to') (fromRight pms)
    fromTeam = getTeam (fromJust fromSquare)
    targetMoves = [ Move (from',to') b | Move (from',to') b <- fromRight pms, to' == to ]
    commitMove mv = Game (getBoardFromMove mv) (tail teams) halfMoveInc fullMoveInc
    halfMoveInc =
      if characterAt fromSquare == Just Pawn
        || not (isNothing (characterAt toSquare)) then
        0
      else
        hmc + 1
    fullMoveInc = case getAffinity fromTeam of
      South -> fmn + 1
      otherwise -> fmn
    promoTarget = [ Move (from',to') b | Move (from',to') b <- fromRight pms,
      getCharacter (fromJust (fromRight (pieceAt to' b))) == fromJust promo ]

play :: Game -> Map.Map Team Mover -> [String] -> IO String
play g@(Game b (t:teams) _ _) moverMap msgs = do
  let
    turnsToTeams' = turnsToTeams teams
    maybeMover = Map.lookup t moverMap
    mover = fromMaybe (error ("No mover for team " ++ show t)) maybeMover
  potMove <- mover b t msgs
  if isNothing potMove then
    return $ intercalate "\n" (boardMessages b turnsToTeams' ++ [teamName t ++ " is a LOSER"])
  else do
    let
      ((from,to),promo) = fromJust potMove
      nextGame = performMove g (from,to) promo
    if isLeft nextGame then
      -- means there was a problem and the returned string is the error msg
      play g moverMap (msgs ++ [fromLeft nextGame])
    else
      let
        Game nextBoard _ _ _ = fromRight nextGame
        boardMessages' = boardMessages nextBoard turnsToTeams'
      in
        play (fromRight nextGame) moverMap boardMessages'

boardMessages :: Board -> [Team] -> [String]
boardMessages b [] = []
boardMessages b (t:teams) =
  filter (not.null) ([ifStalemate,ifCheckmate,ifCurInCheck] ++ boardMessages b teams)
  where
    isStalemate' = null (myPossibleMoves t b) && not (isKingInCheck t b 1)
    isCheckmate' = null (myPossibleMoves t b) && isKingInCheck t b 1
    ifStalemate = if isStalemate' then teamName t ++ " is in stalemate!" else ""
    ifCheckmate = if isCheckmate' then teamName t ++ " is in checkmate!" else ""
    ifCurInCheck = if not isCheckmate' && isKingInCheck t b 1 then teamName t ++ " is in check!" else ""

isStalemate :: Game -> Bool
isStalemate (Game b (t:_) _ _) = null (myPossibleMoves t b) && not (isKingInCheck t b 1)
isStalemate _ = error "No teams specified"

isCheckmate :: Game -> Bool
isCheckmate (Game b (t:_) _ _) = null (myPossibleMoves t b) && isKingInCheck t b 1
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
currentTeam (Game _ [] _ _) = error "No teams defined"
currentTeam (Game _ (t:_) _ _) = t
