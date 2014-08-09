module Hchess.Moves where

import Hchess.Board
import qualified Data.Map as Map
import Data.Char (chr,ord)
import Data.List (sort)
import Data.List.Split (splitOn)


possibleMovesFromLocation :: Board -> Location -> Either String [Location]
possibleMovesFromLocation b l = possibleMoves b l (pieceAt l b)

possibleMoves :: Board -> Location -> Either String Square -> Either String [Location]
possibleMoves _ _ (Left err) = Left err 
possibleMoves b l (Right (Just x)) = Right (possibleMovesByPiece b l x)
possibleMoves _ _ (Right Nothing) = Left "Location is empty"

relLoc :: Location -> Affinity -> (Int,Int) -> Location
relLoc (x,y) North (r,f) = (x+r,y+f)
relLoc (x,y) East (r,f) = (x+f,y-r)
relLoc (x,y) South (r,f) = (x-r,y-f)
relLoc (x,y) West (r,f) = (x-f,y+r)

possibleMovesByPiece :: Board -> Location -> Piece -> [Location]

-- Pawn
possibleMovesByPiece (Board m capt) l (Piece (Team aff t) Pawn ls) =
  let 
    straight = if ls == [] then 
      lineOfSightUnoccupied (Board m capt) [fwd 1,fwd 2] 
    else 
      filterUnoccupied (Board m capt) [fwd 1]
    diag = filterOccupiedByEnemy (Board m capt) (Team aff t) [fl 1, fr 1]
  in sort (straight ++ diag)
  where 
    fwd n = relLoc l aff (0,n)
    fl n = relLoc l aff (-1 * n,n)
    fr n = relLoc l aff (n,n)


-- One last catch-all for unknown pieces
possibleMovesByPiece _ _ p = error ("Piece not yet handled: " ++ (show p))

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




