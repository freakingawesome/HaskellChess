module Hchess.Board where

import qualified Data.Map as Map
import Data.Char (chr,ord)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

data Character = 
  Pawn | Rook | Knight | Bishop | Queen | King
  deriving (Show,Eq)

data Affinity = 
  North | East | South | West
  deriving (Show,Eq,Ord)

data Team = 
  Team Affinity String 
  deriving (Show,Eq)

type Location = (Int,Int)

data Piece = 
  Piece Team Character [Location]
  deriving (Show,Eq)

type CapturedPieceMap = Map.Map Team [Piece]

type Square = Maybe Piece

data Board = 
  Board (Map.Map Location Square) CapturedPieceMap
  deriving (Show,Eq)

emptyBoard :: Int -> Int -> Board
emptyBoard w h = 
  Board (Map.fromList [ ((x,y), Nothing) | x <- [0..(w-1)], y <- [0..(h-1)] ]) Map.empty

newBoard :: Int -> Int -> [(Team,String)] -> Board
newBoard w h [] = emptyBoard w h
newBoard w h (x:xs) = placeTeam (newBoard w h xs) x

placeTeam :: Board -> (Team,String) -> Board
placeTeam b (t,s) = placeTeamPlayers b (t,splitOn " " s)

placeTeamPlayers :: Board -> (Team,[String]) -> Board
placeTeamPlayers b (t,[]) = b
placeTeamPlayers b (t,(p:ps)) = 
  let (character,location) = fromAlgebraicCharacterLocation p
  in placePiece (placeTeamPlayers b (t,ps)) location (Piece t character [])

placePiece :: Board -> Location -> Piece -> Board
placePiece (Board m capt) (x,y) p = 
  let 
    insertOrFail val = 
      if val == Nothing then 
        Just (Just p) 
      else 
        error "Square is already occupied"
  in Board (Map.update insertOrFail (x,y) m) capt

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


pieceAt :: Location -> Board -> Square
pieceAt (x,y) (Board m _) = 
  case Map.lookup (x,y) m of 
    Nothing -> Nothing
    Just x -> x


----------------------------
possibleMoves :: Board -> Location -> [Location]
possibleMoves b l =
  let p = pieceAt l b
  in 
    if p == Nothing then 
      error "No piece at this location" 
    else 
      possibleMovesByPiece b l (fromJust p) 

relLoc :: Location -> Affinity -> (Int,Int) -> Location
relLoc (x,y) North (r,f) = (x+r,y+f)
relLoc (x,y) East (r,f) = (x+f,y-r)
relLoc (x,y) South (r,f) = (x-r,y-f)
relLoc (x,y) West (r,f) = (x-f,y+r)

possibleMovesByPiece :: Board -> Location -> Piece -> [Location]

-- Pawn
possibleMovesByPiece (Board m capt) l (Piece (Team aff tname) Pawn ls) =
  let 
    newpos = if ls == [] then 
      [fwd 1,fwd 2] 
    else 
      [fwd 1]
  in filterUnoccupied (Board m capt) newpos
  where fwd n = relLoc l aff (0, n)



-- One last catch-all for unknown pieces
possibleMovesByPiece _ _ p = error ("Piece not yet handled: " ++ (show p))


filterUnoccupied :: Board -> [Location] -> [Location]
filterUnoccupied b ls = filter (\x -> pieceAt x b == Nothing) ls















