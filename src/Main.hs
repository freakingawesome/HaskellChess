module Main where

-- import Data.Map (fromList)
import qualified Data.Map as Map
-- import Data.Either.Unwrap
-- import Data.Maybe

import Hchess.Board
import Hchess.Moves
import Hchess.Game

import Data.Maybe(fromJust)
-- import Data.List(sort)
import Data.Either.Unwrap

main :: IO ()
main = do
  putStrLn (utf8Board b)
  where
    Game b _ = newStandardGame

utf8Board :: Board -> String
utf8Board (Board m c bs) = boardRows (Board m c bs) length height
  where
    locs = map fst (Map.toList m)
    (length,height) = maximum locs

boardRows :: Board -> Int -> Int -> String
boardRows _ _ (-1) = ""
boardRows b l h = (boardRow b l h) ++ "\n" ++ boardRows b l (h - 1)

boardRow :: Board -> Int -> Int -> String
boardRow b l y = boardSquare b 0 l y

boardSquare :: Board -> Int -> Int -> Int -> String
boardSquare b x maxX y =
  if x > maxX then
    ""
  else
    (contents (fromRight piece)) ++ boardSquare b (x+1) maxX y
  where
    contents p -- =   -- "(" ++ (show x) ++ "," ++ (show y) ++ ") | "
      | p == Nothing = "  ."
      | otherwise = " " ++ [utf8Piece (fromJust p)] ++ " "
    piece = pieceAt (x,y) b

utf8Piece :: Piece -> Char
utf8Piece (Piece (Team _ name) Pawn _) = 
  case name of
    "White" -> '♙'
    "Black" -> '♟'
    _ -> 'p'

utf8Piece (Piece (Team _ name) Rook _) = 
  case name of
    "White" -> '♖'
    "Black" -> '♜'
    _ -> 'R'

utf8Piece (Piece (Team _ name) Knight _) = 
  case name of
    "White" -> '♘'
    "Black" -> '♞'
    _ -> 'N'

utf8Piece (Piece (Team _ name) Bishop _) = 
  case name of
    "White" -> '♗'
    "Black" -> '♝'
    _ -> 'B'

utf8Piece (Piece (Team _ name) Queen _) = 
  case name of
    "White" -> '♕'
    "Black" -> '♛'
    _ -> 'Q'

utf8Piece (Piece (Team _ name) King _) = 
  case name of
    "White" -> '♔'
    "Black" -> '♚'
    _ -> 'K'

black :: Team
black = Team South "Black"

white :: Team
white = Team North "White"

