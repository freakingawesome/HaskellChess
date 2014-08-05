module Hchess.Board (Board(Board),emptyBoard,toAlgebraic,fromAlgebraic) where
import qualified Data.Map as Map
import Data.Char (chr,ord)

data Character = Pawn | Rook | Knight | Bishop | Queen | King
                 deriving (Show)

data Affinity = North | East | South | West
                deriving (Show)

data Team = Team Affinity String deriving (Show)

type Location = (Int,Int)

data Move = Move Location Location Piece
            deriving (Show)

data Piece = Piece Team Character [Move]
             deriving (Show)

data Square = Empty | Square Piece
              deriving (Show)

data Board = Board (Map.Map Location Square)
             deriving (Show)

emptyBoard :: Int -> Int -> Board
emptyBoard w h = Board (Map.fromList [ ((x,y), Empty) | x <- [0..(w-1)], y <- [0..(h-1)] ])


toAlgebraic :: Location -> String
toAlgebraic (x,y) = charToString (chr (x + 97)) ++ show (y + 1)
                    where charToString c = [c]

fromAlgebraic :: String -> Location
fromAlgebraic (x:y) = ((ord x) - 97, (read y :: Int) - 1)


