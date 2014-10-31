module Hchess.Game where

import Hchess.Board
import Hchess.Moves
import qualified Data.Map as Map
import Data.Either.Unwrap
import Data.Maybe
import Data.List(intercalate)

data Game = Game Board [Player]  deriving (Eq,Show)

type Mover = (Board -> Team -> [String] -> IO (Maybe ((Location,Location),Maybe Character)))

data Player = Player Team Mover

instance Show Player where
  show (Player t _) = show t

instance Eq Player where
  (Player t1 _) == (Player t2 _) = t1 == t2

instance Ord Player where
  (Player t1 _) `compare` (Player t2 _) = t1 `compare` t2

newGame :: Int -> Int -> [(Player,String)] -> Game
newGame _ _ [] = error "You must have at least two players"
newGame _ _ [t] = error "You must have at least two players"
newGame w h playerPlacement =
  Game (newBoard w h teamPlacement) (getTurns (map fst playerPlacement))
  where
    teamPlacement = map (\(Player t _,plc) -> (t,plc)) playerPlacement

white :: Team
white = Team North "White"

black :: Team
black = Team South "Black"

newStandardGame :: Mover -> Mover -> Game
newStandardGame whiteMvr blackMvr =
  Game (newStandardBoard white black) (getTurns [Player white whiteMvr,Player black blackMvr])

performMove :: Game -> (Location,Location) -> Maybe Character -> Either String Game
performMove (Game b players) (from,to) promo
  | isLeft fromContents = Left "Invalid location"
  | isNothing fromSquare = Left "The source square is empty"
  | fromTeam /= currentTeam (Game b players) = Left $ "It is not " ++ teamName fromTeam ++ "'s turn"
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
    commitMove mv = Game (getBoardFromMove mv) (tail players)
    promoTarget = [ Move (from',to') b | Move (from',to') b <- fromRight pms,
      getCharacter (fromJust (fromRight (pieceAt to' b))) == fromJust promo ]
    
play :: Game -> [String] -> IO String
play (Game b ((Player t mover):players)) msgs = do
  let
    turnsToTeams' = turnsToTeams (map playerTeam players)
  potMove <- mover b t msgs
  if isNothing potMove then
    return $ intercalate "\n" (boardMessages b turnsToTeams' ++ [(teamName t) ++ " is a LOSER"])
  else do
    let
      ((from,to),promo) = fromJust potMove
      g = Game b ((Player t mover):players)
      nextGame = performMove g (from,to) promo
    if isLeft nextGame then
      -- means there was a problem and the returned string is the error msg
      play g (msgs ++ [(fromLeft nextGame)])
    else
      let
        Game nextBoard _ = fromRight nextGame
        boardMessages' = boardMessages nextBoard turnsToTeams'
      in
        play (fromRight nextGame) boardMessages'

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
isStalemate (Game b ((Player t _):_)) = null (myPossibleMoves t b) && not (isKingInCheck t b 1)
isStalemate _ = error "No teams specified"

isCheckmate :: Game -> Bool
isCheckmate (Game b ((Player t _):_)) = null (myPossibleMoves t b) && isKingInCheck t b 1
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
currentTeam (Game _ ((Player t _):_)) = t

playerTeam :: Player -> Team
playerTeam (Player t _) = t

