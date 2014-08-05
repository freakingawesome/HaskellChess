module Hchess.Board (Board(Board),
  emptyBoard,
  toAlgebraicLocation,
  fromAlgebraicLocation,
  Character(Pawn,Rook,Knight,Bishop,Queen,King),
  Team(Team),
  fromAlgebraicCharacter,
  fromAlgebraicCharacterLocation) where

import qualified Data.Map as Map
import Data.Char (chr,ord)

data Character = Pawn | Rook | Knight | Bishop | Queen | King
                 deriving (Show,Eq)

data Affinity = North | East | South | West
                deriving (Show,Eq)

data Team = Team Affinity String 
            deriving (Show,Eq)

type Location = (Int,Int)

data Piece = Piece Team Character [Location]
             deriving (Show,Eq)

data Square = Empty | Square Piece
              deriving (Show)

data Board = Board (Map.Map Location Square)
             deriving (Show)

emptyBoard :: Int -> Int -> Board
emptyBoard w h = Board (Map.fromList [ ((x,y), Empty) | x <- [0..(w-1)], y <- [0..(h-1)] ])

--newBoard :: Int -> Int -> [(Team,String)] -> Board


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
