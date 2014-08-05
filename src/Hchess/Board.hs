module Hchess.Board (Board(Board),emptyBoard) where
import qualified Data.Map as Map

data Character = Pawn | Rook | Knight | Bishop | Queen | King
                 deriving (Show)

data Affinity = North | East | South | West
                   deriving (Show)

type Location = (Int,Int)

data Move = Move Location Location Piece
            deriving (Show)

data Piece = Piece Affinity Character [Move]
             deriving (Show)

data Square = Empty | Square Piece
              deriving (Show)

data Board = Board (Map.Map Location Square)
             deriving (Show)

emptyBoard :: Int -> Int -> Board
emptyBoard w h = Board (Map.fromList [ ((x,y), Empty) | x <- [0..(w-1)], y <- [0..(h-1)] ])

