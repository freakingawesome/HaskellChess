module Hchess.Moves where

import Hchess.Board
import qualified Data.Map as Map
import Data.Either.Unwrap
import Data.Maybe

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
    p = Piece (Team aff t) Pawn ls
    straight = if null ls then 
      lineOfSightUnoccupied b [fwd' 1,fwd' 2] 
    else 
      filterUnoccupied b [fwd' 1]
    diag = filterOccupiedByEnemy b (Team aff t) [fwdl' 1, fwdr' 1]
  in getMoves b l (straight ++ diag ++ getEnPassantTargetLocations b l p)
  where
    fwd' = fwd l aff
    fwdl' = fwdl l aff
    fwdr' = fwdr l aff

-- One last catch-all for unknown pieces
possibleMovesByPiece _ _ p = error ("Piece not yet handled: " ++ show p)

getMoves :: Board -> Location -> [Location] -> [Move]
getMoves _ _ [] = []
getMoves b from (to:tos) = move b (from,to) : getMoves b from tos

-- Performs an already vetted move.
move :: Board -> (Location,Location) -> Move
move (Board m capt bs) (from,to) = Move (from,to) b''''
  where
    (b',Just mover) = pickUpPiece (Board m capt bs) from
    moverChar (Piece _ c _) = c
    moverAff (Piece (Team aff _) _ _) = aff
    moverTeam (Piece t _ _) = t
    (b'',dead) = 
      pickUpPiece b' (
        if isEnPassantCapture then 
          relLoc to (moverAff mover) (0,-1) 
        else 
          to)
    b''' = recordCapture b'' (getTeam mover) dead
    b'''' = recordLastBoard (placePiece b''' to mover) (Board m capt bs)
    isEnPassantCapture = 
      moverChar mover == Pawn 
      && (to == relLoc from (moverAff mover) (1,1) || to == relLoc from (moverAff mover) (-1,1))
      && isEmpty (pieceAt to b')
      && isEnemyPawn (pieceAt (relLoc to (moverAff mover) (0,-1)) b') (moverTeam mover) 

pickUpPiece :: Board -> Location -> (Board,Maybe Piece)
pickUpPiece (Board m capt bs) l = (Board (Map.update removePiece l m) capt bs, p')
  where
    p = fromRight (pieceAt l (Board m capt bs))
    removePiece _ = Just Nothing 
    recordLastLocation (Piece t c ls) = Piece t c (ls ++ [l])
    p' = 
      if isNothing p then 
        Nothing 
      else 
        Just (recordLastLocation (fromJust p))

filterUnoccupied :: Board -> [Location] -> [Location]
filterUnoccupied b = filter (\x -> pieceAt x b == Right Nothing)

filterOccupiedByEnemy :: Board -> Team -> [Location] -> [Location]
filterOccupiedByEnemy b t = filter (\x -> isEnemy t (pieceAt x b))

isEnemy :: Team -> Either String Square -> Bool
isEnemy us (Right (Just (Piece t _ _))) = us /= t
isEnemy _ _ = False

-- Assumes head to tail is a single line of sight
lineOfSightUnoccupied :: Board -> [Location] -> [Location]
lineOfSightUnoccupied _ [] = []
lineOfSightUnoccupied b (l:ls) = 
  if pieceAt l b == Right Nothing then 
    l:lineOfSightUnoccupied b ls
  else 
    []

-- Places the piece on the end of the board's captured list
recordCapture :: Board -> Team -> Maybe Piece -> Board
recordCapture b _ Nothing = b
recordCapture (Board m capt bs) t (Just p)
  | isNothing existingTeam = Board m (Map.fromList [(t,[p])]) bs
  | otherwise = Board m (Map.update appendPiece t capt) bs
  where 
    existingTeam = Map.lookup t capt
    appendPiece capts = Just (capts ++ [p])

recordLastBoard :: Board -> Board -> Board
recordLastBoard (Board m capt bs) old = Board m capt (bs ++ [old])

-- assumes the location you send in is a pawn, this will return the forward left/right positions
-- if en passant is possible for either side.
getEnPassantTargetLocations :: Board -> Location -> Piece -> [Location]
getEnPassantTargetLocations (Board _ _ []) _ _ = []
getEnPassantTargetLocations (Board m capt bs) l (Piece (Team aff t) Pawn ls) =
  ep [rgt' 1,fwdr' 1,ffr] ++ ep [lft' 1,fwdl' 1,ffl]
  where
    fwd' = fwd l aff
    fwdl' = fwdl l aff
    fwdr' = fwdr l aff
    rgt' = rgt l aff
    lft' = lft l aff
    ffr = relLoc l aff (1,2)
    ffl = relLoc l aff (-1,2)
    b = Board m capt bs
    lastb = last bs
    ep locs = [locs!!1 | 
      isEnemyPawn (pieceAt (head locs) b) (Team aff t) 
      && isEmpty (pieceAt (locs!!1) b) 
      && isEmpty (pieceAt (locs!!2) b) 
      && isEnemyPawn (pieceAt (locs!!2) lastb) (Team aff t) 
      && isEmpty (pieceAt (locs!!1) lastb) 
      && isEmpty (pieceAt (head locs) lastb)]

getEnPassantTargetLocations _ _ _ = []

isEnemyPawn :: Either String Square -> Team -> Bool
isEnemyPawn (Right (Just (Piece otherTeam Pawn _))) t = otherTeam /= t
isEnemyPawn _ _ = False

isEmpty :: Either String Square -> Bool
isEmpty (Right Nothing) = True
isEmpty _ = False

