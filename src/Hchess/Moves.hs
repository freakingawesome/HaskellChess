module Hchess.Moves where

import Hchess.Board
import qualified Data.Map as Map
import Data.Maybe(fromJust)
import Data.Either.Unwrap
data Move = Move (Location,Location) Board deriving (Show,Eq)

possibleMovesFromLocation :: Board -> Location -> Either String [Move]
possibleMovesFromLocation b l = possibleMoves b l (pieceAt l b)

possibleMoves :: Board -> Location -> Either String Square -> Either String [Move]
possibleMoves _ _ (Left err) = Left err 
possibleMoves b l (Right (Just x)) = Right (possibleMovesByPiece b l x)
possibleMoves _ _ (Right Nothing) = Left "Location is empty"

relLoc :: Location -> Affinity -> (Int,Int) -> Location
relLoc (x,y) North (r,f) = (x+r,y+f)
relLoc (x,y) East (r,f) = (x+f,y-r)
relLoc (x,y) South (r,f) = (x-r,y-f)
relLoc (x,y) West (r,f) = (x-f,y+r)

-- Shorthand for affinity-relative moves
fwd :: Location -> Affinity -> Int -> (Int,Int)
fwd l aff n = relLoc l aff (0,n)

fwdl :: Location -> Affinity -> Int -> (Int,Int)
fwdl l aff n = relLoc l aff (-n,n)

fwdr :: Location -> Affinity -> Int -> (Int,Int)  
fwdr l aff n = relLoc l aff (n,n)

rgt :: Location -> Affinity -> Int -> (Int,Int)  
rgt l aff n = relLoc l aff (n,0)

lft :: Location -> Affinity -> Int -> (Int,Int)
lft l aff n = relLoc l aff (-n,0)

rev :: Location -> Affinity -> Int -> (Int,Int)
rev l aff n = relLoc l aff (0,-n)

revl :: Location -> Affinity -> Int -> (Int,Int)
revl l aff n = relLoc l aff (-n,-n)

revr :: Location -> Affinity -> Int -> (Int,Int)  
revr l aff n = relLoc l aff (n,-n)

-- List out all possible moves per piece
possibleMovesByPiece :: Board -> Location -> Piece -> [Move]

-- Pawn
possibleMovesByPiece (Board m capt bs) l (Piece (Team aff t) Pawn ls) =
  let 
    b = Board m capt bs
    lastb = last bs
    straight = if ls == [] then 
      lineOfSightUnoccupied b [fwd' 1,fwd' 2] 
    else 
      filterUnoccupied b [fwd' 1]
    diag = filterOccupiedByEnemy b (Team aff t) [fwdl' 1, fwdr' 1]
    enpassr = if fwdr' 1 `notElem` diag && isEnemyPawn (pieceAt (rgt' 1) b) && isEmpty (pieceAt (fwdr' 1) b) && isEmpty (pieceAt ffr b) && isEnemyPawn (pieceAt ffr lastb) && isEmpty (pieceAt (fwdr' 1) lastb) && isEmpty (pieceAt (rgt' 1) lastb) then [fwdr' 1] else []
      
  in getMoves b l (straight ++ diag ++ enpassr)
  where
    fwd' = fwd l aff
    fwdl' = fwdl l aff
    fwdr' = fwdr l aff
    rgt' = rgt l aff
    lft' = lft l aff
    ffr = relLoc l aff (1,2)
    isEnemyPawn (Right (Just (Piece otherTeam Pawn _))) = otherTeam /= (Team aff t)
    isEnemyPawn _ = False
    isEmpty (Right Nothing) = True
    isEmpty _ = False

-- One last catch-all for unknown pieces
possibleMovesByPiece _ _ p = error ("Piece not yet handled: " ++ (show p))

getMoves :: Board -> Location -> [Location] -> [Move]
getMoves _ _ [] = []
getMoves b from (to:tos) = move b (from,to) : getMoves b from tos

-- Performs an already vetted move.
move :: Board -> (Location,Location) -> Move
move (Board m capt bs) (from,to) = Move (from,to) b''''
  where
    (b',Just mover) = pickUpPiece (Board m capt bs) from
    (b'',dead) = pickUpPiece b' to 
    b''' = recordCapture b'' (getTeam mover) dead
    b'''' = recordLastBoard (placePiece b''' to mover) (Board m capt bs)

pickUpPiece :: Board -> Location -> (Board,Maybe Piece)
pickUpPiece (Board m capt bs) l = (Board (Map.update removePiece l m) capt bs, p')
  where
    p = fromRight (pieceAt l (Board m capt bs))
    removePiece _ = Just Nothing 
    recordLastLocation (Piece t c ls) = Piece t c (ls ++ [l])
    p' = 
      if p == Nothing then 
        Nothing 
      else 
        Just (recordLastLocation (fromJust p))

filterUnoccupied :: Board -> [Location] -> [Location]
filterUnoccupied b ls = filter (\x -> pieceAt x b == Right Nothing) ls

filterOccupiedByEnemy :: Board -> Team -> [Location] -> [Location]
filterOccupiedByEnemy b t ls = filter (\x -> isEnemy t (pieceAt x b)) ls

isEnemy :: Team -> Either String Square -> Bool
isEnemy us (Right (Just (Piece t _ _))) = us /= t
isEnemy _ _ = False

-- Assumes head to tail is a single line of sight
lineOfSightUnoccupied :: Board -> [Location] -> [Location]
lineOfSightUnoccupied _ [] = []
lineOfSightUnoccupied b (l:ls) = 
  if pieceAt l b == Right Nothing then 
    l:(lineOfSightUnoccupied b ls) 
  else 
    []

-- Places the piece on the end of the board's captured list
recordCapture :: Board -> Team -> Maybe Piece -> Board
recordCapture b _ Nothing = b
recordCapture (Board m capt bs) t (Just p)
  | existingTeam == Nothing = Board m (Map.fromList [(t,[p])]) bs
  | otherwise = Board m (Map.update appendPiece t capt) bs
  where 
    existingTeam = Map.lookup t capt
    appendPiece capts = Just (capts ++ [p])

recordLastBoard :: Board -> Board -> Board
recordLastBoard (Board m capt bs) old = Board m capt (bs ++ [old])

