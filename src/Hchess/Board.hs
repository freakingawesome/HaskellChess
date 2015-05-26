module Hchess.Board where

import qualified Data.Map as Map
import Data.Char (chr,ord)
import Data.List (sort,nub)
import Data.List.Split (splitOn)
import Data.Maybe
import qualified Data.HashSet as HS

data Character =
  Pawn | Rook | Knight | Bishop | Queen | King
  deriving (Show,Read,Eq,Ord)

data Affinity =
  North | East | South | West
  deriving (Show,Read,Eq,Ord)

data Team =
  Team Affinity String
  deriving (Show,Read,Eq,Ord)

type Location = (Int,Int)

data Piece =
  Piece Team Character [Location]
  deriving (Show,Read,Eq)

type Square = Maybe Piece

data Board =
  Board {
    squares :: Map.Map Location Square,
    history :: [Board],
    castlingRookAvailability :: HS.HashSet Location,
    enPassantTarget :: Maybe Location
  }
  deriving (Show,Eq)

emptyBoard :: Int -> Int -> Board
emptyBoard w h =
  Board (Map.fromList [ ((x,y), Nothing) | x <- [0..(w-1)], y <- [0..(h-1)] ]) [] HS.empty Nothing

newBoard :: Int -> Int -> [(Team,String)] -> Board
newBoard w h [] = emptyBoard w h
newBoard w h (x:xs) = placeTeam (newBoard w h xs) x

placeTeam :: Board -> (Team,String) -> Board
placeTeam b (t,s) = placeTeamPlayers b (t,splitOn " " s)

placeTeamPlayers :: Board -> (Team,[String]) -> Board
placeTeamPlayers b (_,[]) = b
placeTeamPlayers b (t,p:ps) =
  let (character,location) = fromAlgebraicCharacterLocation p
  in placePiece (placeTeamPlayers b (t,ps)) location (Piece t character [])

placePiece :: Board -> Location -> Piece -> Board
placePiece (Board m bs ept cra) (x,y) p =
  let
    insertOrFail val =
      if isNothing val then
        Just (Just p)
      else
        error $ "Square " ++ toAlgebraicLocation (x,y) ++ " is already occupied"
  in Board (Map.update insertOrFail (x,y) m) bs ept cra

newStandardBoard :: Team -> Team -> Board
newStandardBoard t1 t2 = newBoard 8 8 [
  (t1,"pa2 pb2 pc2 pd2 pe2 pf2 pg2 ph2 Ra1 Nb1 Bc1 Qd1 Ke1 Bf1 Ng1 Rh1"),
  (t2,"pa7 pb7 pc7 pd7 pe7 pf7 pg7 ph7 Ra8 Nb8 Bc8 Qd8 Ke8 Bf8 Ng8 Rh8")
  ]

toAlgebraicLocation :: Location -> String
toAlgebraicLocation (x,y) = charToString (chr (x + 97)) ++ show (y + 1)
  where charToString c = [c]

fromAlgebraicLocation :: String -> Location
fromAlgebraicLocation [] = error "Invalid notation"
fromAlgebraicLocation (x:y) = (ord x - 97, (read y :: Int) - 1)

fromAlgebraicCharacter :: Char -> Character
fromAlgebraicCharacter c
  | c == 'p' = Pawn
  | c == 'R' = Rook
  | c == 'N' = Knight
  | c == 'B' = Bishop
  | c == 'Q' = Queen
  | c == 'K' = King
  | otherwise = error "Invalid character"

fromAlgebraicCharacterLocation :: String -> (Character,Location)
fromAlgebraicCharacterLocation [] = error "Invalid character location"
fromAlgebraicCharacterLocation (x:xs) =
  (fromAlgebraicCharacter x,fromAlgebraicLocation xs)

pieceAt :: Location -> Board -> Either String Square
pieceAt (x,y) (Board m _ _ _) =
  case Map.lookup (x,y) m of
    Nothing -> Left "Invalid location"
    Just a -> Right a

getTeam :: Piece -> Team
getTeam (Piece t _ _) = t

remainingTeams :: Board -> [Team]
remainingTeams (Board m _ _ _) =
  nub [ getTeam (fromJust square) | (_,square) <- Map.toList m, isJust square ]

teamName :: Team -> String
teamName (Team _ name) = name

getCharacter :: Piece -> Character
getCharacter (Piece _ c _) = c

getAffinity :: Team -> Affinity
getAffinity (Team a _) = a

