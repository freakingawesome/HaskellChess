module Main where

-- import Data.Map (fromList)
import qualified Data.Map as Map
-- import Data.Either.Unwrap
-- import Data.Maybe

import Hchess.Board
import Hchess.Moves
import Hchess.Game

import Data.Maybe(fromJust)
import Data.List(sort)
import Data.List.Split(splitOn)
import Data.Either.Unwrap
import Text.Regex.Posix

main :: IO ()
main = do
  putStrLn (utf8Game g)
  getUserInput g
  return ()
  where g = newStandardGame

getUserInput :: Game -> IO ()
getUserInput g = do
  input <- getLine
  case head (splitOn " " input) of
    "?" -> showHelp
    "help" -> showHelp
    "exit" -> putStrLn "Goodbye"
    "board" -> do
      putStrLn (utf8Game g)
      getUserInput g
    "mv" ->
      let
        (_,_,_,mvs) = input =~ "^mv ([a-h][1-8]) ([a-h][1-8])$" :: (String,String,String,[String])
      in do
      if null mvs then do
        putStrLn "Parse error. Should be something like: mv a2 a3"
        getUserInput g
      else
        let
          (from,to) = (fromAlgebraicLocation (head mvs),fromAlgebraicLocation (mvs!!1))
        in
          let
            mv = performMove g (from,to) Nothing
          in
            if isLeft mv then do
              putStrLn (fromLeft mv)
              getUserInput g
            else do
              putStrLn (utf8Game (fromRight mv))
              getUserInput (fromRight mv)
    "pm" -> do
      putStrLn "Show possible moves"
      getUserInput g
    _ -> do
      putStrLn "Huh?"
      getUserInput g
  return ()
  where
    showHelp = do
      putStrLn "\n\
        \When you see <loc>, use algebraic notation. For example, a1 is the lower left\n\
        \corner and h8 is the upper right.\n\n\
        \pm <loc>         Shows possible move from location\n\
        \mv <loc> <loc>   Attempts to move the piece at the first loc to the second\n"
      getUserInput g

utf8Game :: Game -> String
utf8Game (Game b (cur:_)) = utf8Board b ++ "\nIt is " ++ teamName cur ++ "'s turn\n"

utf8Board :: Board -> String
utf8Board (Board m c bs) = boardRows (Board m c bs) len hgt
  where
    locs = map fst (Map.toList m)
    (len,hgt) = maximum locs

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

