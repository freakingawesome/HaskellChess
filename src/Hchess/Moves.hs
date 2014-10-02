module Hchess.Moves where

import Hchess.Board
import qualified Data.Map as Map
import Data.Either.Unwrap
import Data.Maybe

data Move = Move (Location,Location) Board deriving (Show,Eq)

-- HACK: This "deep" parameter allows us to stop checking possible boards for king-in-check. It 
-- should probably be based on team count, and there's probably a better way to do this, but hey,
-- the tests pass for now.
possibleMovesFromLocation :: Board -> Location -> Int -> Either String [Move]
possibleMovesFromLocation b l = possibleMoves b l (pieceAt l b) 

possibleMoves :: Board -> Location -> Either String Square -> Int -> Either String [Move]
possibleMoves _ _ (Left err) _ = Left err 
possibleMoves b l (Right (Just (Piece t c ls))) deep = Right (protectKing moves)
  where 
    moves = possibleMovesByPiece b l (Piece t c ls)
    protectKing ms = [ Move m b' | Move m b' <- ms, not (isKingInCheck t b' deep)]

possibleMoves _ _ (Right Nothing) _ = Left "Location is empty"

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

ffr :: Location -> Affinity -> (Int,Int)
ffr l aff = relLoc l aff (1,2)

ffl :: Location -> Affinity -> (Int,Int)
ffl l aff = relLoc l aff (-1,2)

-- List out all possible moves per piece
possibleMovesByPiece :: Board -> Location -> Piece -> [Move]

-- Pawn
possibleMovesByPiece (Board m capt bs) l (Piece (Team aff t) Pawn ls) =
  let 
    b = Board m capt bs
    p = Piece (Team aff t) Pawn ls
    straight = if null ls then 
      lineOfSightUnoccupied b [fwd l aff 1,fwd l aff 2] 
    else 
      filterUnoccupied b [fwd l aff 1]
    diag = filterOccupiedByEnemy b (Team aff t) [fwdl l aff 1, fwdr l aff 1]
  in getMoves b l (straight ++ diag ++ getEnPassantTargetLocations b l p)

-- Rook
possibleMovesByPiece b l (Piece t Rook _) =
  getMoves b l (concatMap (lineOfSightMaybeCapture b t l) [(0,1),(1,0),(0,-1),(-1,0)])

-- Knight
possibleMovesByPiece b l (Piece (Team aff t) Knight _) =
  getMoves b l (
    emptyOrEnemy b (Team aff t) (map (relLoc l aff) [
      (1,2),
      (1,-2),
      (2,1),
      (2,-1),
      (-1,2),
      (-1,-2),
      (-2,1),
      (-2,-1)]))

-- Bishop
possibleMovesByPiece b l (Piece t Bishop _) =
  getMoves b l (concatMap (lineOfSightMaybeCapture b t l) [(1,1),(1,-1),(-1,1),(-1,-1)])

-- Queen
possibleMovesByPiece b l (Piece t Queen _) =
  getMoves b l (concatMap (lineOfSightMaybeCapture b t l) [
    (0,1),
    (1,0),
    (0,-1),
    (-1,0),
    (1,1),
    (1,-1),
    (-1,1),
    (-1,-1)])

-- King
possibleMovesByPiece b l (Piece (Team aff t) King ms) =
  getMoves b l (
    emptyOrEnemy b (Team aff t) (map (relLoc l aff) ([
      (0,1),
      (1,0),
      (0,-1),
      (-1,0),
      (1,1),
      (1,-1),
      (-1,1),
      (-1,-1)] ++ getCastlingPositionsFromKing)))
  where
    getCastlingPositionsFromKing = 
      if null ms then
        []
      else
        castLeft ++ castRight
    castLeft = [(-2,0) | not (null (lineOfSightEndingInUnmovedTeamRook b (Team aff t) l (-1,0)))]
    castRight = [(2,0) | not (null (lineOfSightEndingInUnmovedTeamRook b (Team aff t) l (1,0)))]

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

-- Walks in a direction until it goes off the board or lands on an enemy
lineOfSightMaybeCapture :: Board -> Team -> Location -> (Int,Int) -> [Location]
lineOfSightMaybeCapture b (Team aff t) l (r,f)
  | p == Right Nothing = targetLoc : lineOfSightMaybeCapture b (Team aff t) targetLoc (r,f)
  | isEnemy (Team aff t) p = [targetLoc]
  | otherwise = []
  where 
    targetLoc = relLoc l aff (r,f)
    p = pieceAt targetLoc b

-- Used for castling
lineOfSightEndingInUnmovedTeamRook :: Board -> Team -> Location -> (Int,Int) -> [Location]
lineOfSightEndingInUnmovedTeamRook b (Team aff t) l (r,f)
  | p == Right Nothing = if null restOfSquares then [] else targetLoc : restOfSquares
  | isTeammateUnmovedRook p (Team aff t) = [targetLoc]
  | otherwise = []
  where 
    targetLoc = relLoc l aff (r,f)
    p = pieceAt targetLoc b
    restOfSquares = lineOfSightEndingInUnmovedTeamRook b (Team aff t) (relLoc l aff (r,f)) (r,f)

emptyOrEnemy :: Board -> Team -> [Location] -> [Location]
emptyOrEnemy _ _ [] = []
emptyOrEnemy b t (l:ls)
  | p == Right Nothing || isEnemy t p = l : emptyOrEnemy b t ls
  | otherwise = emptyOrEnemy b t ls
  where p = pieceAt l b

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
getEnPassantTargetLocations (Board m capt bs) l (Piece (Team aff t) Pawn _) =
  ep [rgt l aff 1,fwdr l aff 1,ffr l aff] ++ ep [lft l aff 1,fwdl l aff 1,ffl l aff]
  where
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

isTeammateUnmovedRook :: Either String Square -> Team -> Bool
isTeammateUnmovedRook (Right (Just (Piece team Rook ms))) t = team == t && null ms
isTeammateUnmovedRook _ _ = False

isKingInCheck :: Team -> Board -> Int -> Bool
isKingInCheck t b deep = deep > 0 && kingLoc `elem` possibleEnemyLocs
  where
    kingLoc = head [ loc | (loc,Piece _ c _) <- mySquares t b, c == King ]
    enemyLocs = [ loc | (loc,_) <- enemySquares t b ]
    possibleEnemyLocs = [ loc | Move (_,loc) _ <- concatMap (\l -> fromRight (possibleMovesFromLocation b l (deep - 1))) enemyLocs ]

mySquares :: Team -> Board -> [(Location,Piece)]
mySquares t (Board m _ _) = 
  [ (loc,fromJust square) | (loc,square) <- Map.toList m, 
    isJust square, 
    getTeam (fromJust square) == t ]

enemySquares :: Team -> Board -> [(Location,Piece)]
enemySquares t (Board m _ _) = 
  [ (loc,fromJust square) | (loc,square) <- Map.toList m, 
    isJust square, 
    getTeam (fromJust square) /= t ]
