module Main where

-- import Data.Map (fromList)
import qualified Data.Map as Map
-- import Data.Either.Unwrap
import Data.Maybe
import Safe

import Hchess.Board
import Hchess.Moves
import Hchess.Game

import Data.Maybe(fromJust)
import Data.List(sort,intercalate)
import Data.List.Split(splitOn)
import Data.Either.Unwrap
import Text.Regex.Posix

main = 
  play g []
  where
    g = newStandardGame consoleMover consoleMover

consoleMover :: Board -> Team -> [String] -> IO (Maybe ((Location,Location),Maybe Character))
consoleMover b t msgs = do
  putStrLn $ utf8Board b []
    ++ "\n"
    ++ if not (null msgs) then intercalate "\n" msgs else ""
  getUserInput b t

getUserInput :: Board -> Team -> IO (Maybe ((Location,Location),Maybe Character))
getUserInput b t = do
  putStrLn $ "It is " ++ teamName t ++ "'s turn\n"
  input <- getLine
  case head (splitOn " " input) of
    "?" -> showHelp
    "help" -> showHelp
    "exit" -> do
      putStrLn "Goodbye"
      return Nothing
    "board" -> do
      putStrLn (utf8Board b [])
      getUserInput b t
    "mv" ->
      let
        (_,_,_,mvs) = input =~ "^mv ([a-h][1-8]) ([a-h][1-8])( [a-zA-Z]+)?$" :: (String,String,String,[String])
      in do
      if null mvs then do
        putStrLn "Parse error. Should be something like: mv a2 a3"
        getUserInput b t
      else
        let
          (from,to,promoText) = (fromAlgebraicLocation (head mvs),fromAlgebraicLocation (mvs!!1),mvs!!2)
        in
          let
            promo = readMay (mvs!!2) :: Maybe Character
          in
            return $ Just ((from,to),promo)
    "pm" ->
      let
        (_,_,_,mvs) = input =~ "^pm ([a-h][1-8])$" :: (String,String,String,[String])
      in do
        if length mvs /= 1 then do
          putStrLn "Parse error. Should be something like: pm a2"
          getUserInput b t
        else
          let
            from = fromAlgebraicLocation (head mvs)
            pms = possibleMovesFromLocation b from 1
            targetLocs = map (\(Move (_,to) _) -> to) (fromRight pms)
          in
            if isLeft pms then do
              putStrLn (fromLeft pms)
              getUserInput b t
            else do
              putStrLn (utf8Board b targetLocs
                ++ "\n"
                ++ head mvs
                ++ " has "
                ++ show (length (fromRight pms))
                ++ " possible moves.\n"
                ++ show (map toAlgebraicLocation targetLocs))
              getUserInput b t
    _ -> do
      putStrLn "Huh?"
      getUserInput b t
  where
    showHelp = do
      putStrLn "\n\
        \When you see <loc>, use algebraic notation. For example, a1 is the lower left\n\
        \corner and h8 is the upper right.\n\n\
        \pm <loc>               Shows possible move from location\n\
        \mv <loc> <loc> [promo] Attempts to move the piece at the first loc to the second.\n\
        \                       The promo field is only required for pawn promotion.\n\
        \board                  Shows the board again.\n\
        \exit                   exit\n"
      getUserInput b t

{-
utf8Game :: Game -> [Location]-> String
utf8Game (Game b (cur:turns)) posMoves = utf8Board b posMoves ++ "\nIt is " ++ teamName cur ++ "'s turn\n" ++ otherMessages
  where
    g = Game b (cur:turns)
    otherMessages = ifStalemate ++ ifCheckmate ++ ifCurInCheck
    ifStalemate = if isStalemate g then teamName cur ++ " is in stalemate!\n" else ""
    ifCheckmate = if isCheckmate g then teamName cur ++ " is in checkmate! " ++ teamName (head turns) ++ " wins!\n" else ""
    ifCurInCheck = if not (isCheckmate g) && isKingInCheck cur b 1 then teamName cur ++ " is in check!\n" else ""
-}

utf8Board :: Board -> [Location] -> String
utf8Board (Board m c bs) posMoves = boardRows (Board m c bs) len hgt posMoves
  where
    locs = map fst (Map.toList m)
    (len,hgt) = maximum locs

boardRows :: Board -> Int -> Int -> [Location] -> String
boardRows _ _ (-1) _ = ""
boardRows b l h posMoves = (boardRow b l h posMoves) ++ "\n" ++ boardRows b l (h - 1) posMoves

boardRow :: Board -> Int -> Int -> [Location] -> String
boardRow b l y posMoves = boardSquare b 0 l y posMoves

boardSquare :: Board -> Int -> Int -> Int -> [Location] -> String
boardSquare b x maxX y posMoves =
  if x > maxX then
    ""
  else
    (contents (fromRight piece)) ++ boardSquare b (x+1) maxX y posMoves
  where
    contents p
      | p == Nothing = lbord ++ "•" ++ rbord
      | otherwise = lbord ++ [utf8Piece (fromJust p)] ++ rbord
    piece = pieceAt (x,y) b
    lbord = if (x,y) `elem` posMoves then "▕" else " "
    rbord = if (x,y) `elem` posMoves then "▏" else " "

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

