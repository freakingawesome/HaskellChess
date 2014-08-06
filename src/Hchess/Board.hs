module Hchess.Board (Board(Board),
  emptyBoard,
  newBoard,
  --newStandardBoard,
  toAlgebraicLocation,
  fromAlgebraicLocation,
  Character(Pawn,Rook,Knight,Bishop,Queen,King),
  Affinity(North,East,South,West),
  Team(Team),
  Location,
  Piece(Piece),
  pieceAt,
  Square,
  fromAlgebraicCharacter,
  fromAlgebraicCharacterLocation) where

import qualified Data.Map as Map
import Data.Char (chr,ord)
import Data.List.Split (splitOn)

data Character = Pawn | Rook | Knight | Bishop | Queen | King
                 deriving (Show,Eq)

data Affinity = North | East | South | West
                deriving (Show,Eq)

data Team = Team Affinity String 
            deriving (Show,Eq)

type Location = (Int,Int)

data Piece = Piece Team Character [Location]
             deriving (Show,Eq)

type Square = Maybe Piece

data Board = Board (Map.Map Location Square)
             deriving (Show,Eq)

emptyBoard :: Int -> Int -> Board
emptyBoard w h = Board (Map.fromList [ ((x,y), Nothing) | x <- [0..(w-1)], y <- [0..(h-1)] ])

newBoard :: Int -> Int -> [(Team,String)] -> Board
newBoard w h [] = emptyBoard w h
--newBoard w h (x:xs) = placeTeam (newBoard w h (splitOn " " xs)) x
newBoard w h (x:xs) = placeTeam (newBoard w h xs) x

placeTeam :: Board -> (Team,String) -> Board
placeTeam b (t,s) = placeTeamPlayers b (t,splitOn " " s)

placeTeamPlayers :: Board -> (Team,[String]) -> Board
placeTeamPlayers b (t,[]) = b
placeTeamPlayers b (t,(p:ps)) = let (character,location) = fromAlgebraicCharacterLocation p
                                in placePiece (placeTeamPlayers b (t,ps)) location (Piece t character [])

placePiece :: Board -> Location -> Piece -> Board
placePiece (Board m) (x,y) p = Board m
--newStandardBoard :: String -> Board
--newStandardBoard [] = emptyBoard 8 8

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
fromAlgebraicCharacterLocation (x:xs) = (fromAlgebraicCharacter x,fromAlgebraicLocation xs)


pieceAt :: Location -> Board -> Square
pieceAt (x,y) (Board m) = case Map.lookup (x,y) m of Nothing -> Nothing
                                                     Just x -> x


