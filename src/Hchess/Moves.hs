module Hchess.Moves where

import Hchess.Board
import qualified Data.Map as Map
import Data.Either.Unwrap
import Data.Maybe
import qualified Data.HashSet as HS

data Move = Move (Location,Location) Board deriving (Show,Eq)

getBoardFromMove :: Move -> Board
getBoardFromMove (Move (_,_) b) = b

-- HACK: This "deep" parameter allows us to stop checking possible boards for king-in-check. It
-- should probably be based on team count, and there's probably a better way to do this, but hey,
-- the tests pass for now.
possibleMovesFromLocation :: Board -> Location -> Int -> Either String [Move]
possibleMovesFromLocation b l = possibleMoves b l (pieceAt l b)

possibleMoves :: Board -> Location -> Either String Square -> Int -> Either String [Move]
possibleMoves _ _ (Left err) _ = Left err
possibleMoves b l (Right (Just p@(Piece t c ls))) deep = Right (protectKing moves)
  where
    moves = possibleMovesByPiece b l p
    protectKing = filter (\(Move m b') -> not (isKingInCheck t b' deep))

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
possibleMovesByPiece (Board m cra ept) l (Piece (Team aff t) Pawn ls) =
  let
    b = Board m cra ept
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
possibleMovesByPiece b@(Board _ cra _) l (Piece team@(Team aff t) King ms) =
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
    getCastlingPositionsFromKing = if not (null castlingLocs)
        && not (isLocationImmediatelyThreatened (Team aff t) b l) then
          castlingLocs
      else
          []
    lineOfSight dir = lineOfSightEndingInUnmovedTeamRook b (Team aff t) l (dir,0)
    castlingPossible dir = not (null (lineOfSight dir))
      && and (first2AndLast1 (map (not.isLocationImmediatelyThreatened (Team aff t) b) (lineOfSight dir)))
    castlingLocs = map (\i -> (2 * i, 0)) $ filter castlingPossible dirs
    first2AndLast1 list = take 2 list ++ [last list]
    dirs = map (\(x,_) -> let n = x - fst l in quot n (abs n)) $
      HS.toList $ HS.filter (\l -> teamAt b l == Just team) cra

getMoves :: Board -> Location -> [Location] -> [Move]
getMoves _ _ [] = []
getMoves b from (to:tos) =
  (if isPawnPromotion then
    pawnPromotionMoves
  else
    [basicMove])
  ++ getMoves b from tos
  where
    basicMove = move b (from,to)
    fromAff (Just (Piece (Team aff _) _ _)) = aff
    fromPiece = fromRight (pieceAt from b)
    isPawn (Just (Piece _ Pawn _)) = True
    isPawn _ = False
    isPawnPromotion = isPawn fromPiece
      && relLoc from (fromAff fromPiece) (0,1) == to
      && isRight (pieceAt to b)
      && isLeft (pieceAt (relLoc to (fromAff fromPiece) (0,1)) b)
    pawnPromotionMoves = [
      swapTargetChar basicMove Rook,
      swapTargetChar basicMove Knight,
      swapTargetChar basicMove Bishop,
      swapTargetChar basicMove Queen
      ]
      where
        swapTargetChar (Move locs b) toChar = Move locs (swapCharAt to b toChar)
        swapCharAt loc (Board m cra ept) toChar = Board (Map.update (swapChar toChar) loc m) cra ept
        swapChar toChar (Just (Piece t c pl)) = Just (Just (Piece t toChar pl))

-- Performs an already vetted move.
move :: Board -> (Location,Location) -> Move
move (Board m cra ept) (from,to) = Move (from,to) b''''
  where
    fromPiece = fromJust (fromRight (pieceAt from (Board m cra ept)))
    (b',Just mover) = pickUpPiece (ifCastlingFirstMoveRook (Board m cra newEnPassantTargetLoc)) from
    moverChar (Piece _ c _) = c
    moverAff (Piece (Team aff _) _ _) = aff
    moverTeam (Piece t _ _) = t
    (b'',dead) =
      pickUpPiece b' (
        if isEnPassantCapture then
          relLoc to (moverAff mover) (0,-1)
        else
          to)
    b''' = placePiece b'' to mover
    b'''' = b''' { castlingRookAvailability = case moverChar fromPiece of
      King -> HS.filter (\l -> teamAt b''' l /= Just (moverTeam fromPiece)) cra
      Rook -> HS.filter (/=from) cra
      otherwise -> cra }
    isEnPassantCapture =
      moverChar mover == Pawn
      && (to == relLoc from (moverAff mover) (1,1) || to == relLoc from (moverAff mover) (-1,1))
      && isEmpty (pieceAt to b')
      && isEnemyPawn (pieceAt (relLoc to (moverAff mover) (0,-1)) b') (moverTeam mover)
    newEnPassantTargetLoc = if
        moverChar mover == Pawn
        && to == relLoc from (moverAff mover) (0,2)
      then Just $ relLoc from (moverAff mover) (0,1)
      else Nothing
    ifCastlingFirstMoveRook initialBoard
      | isCastling (-2) = getBoardFromMove (move initialBoard (locOfCastlingRook (-1)))
      | isCastling 2 = getBoardFromMove (move initialBoard (locOfCastlingRook 1))
      | otherwise = initialBoard
      where
        isCastling horizJump =
          moverChar fromPiece == King
          && relLoc from (getAffinity (getTeam fromPiece)) (horizJump,0) == to
        locOfCastlingRook sign = (
          last (lineOfSightEndingInUnmovedTeamRook initialBoard (getTeam fromPiece) from (sign,0)),
          relLoc from (getAffinity (getTeam fromPiece)) (sign,0))

pickUpPiece :: Board -> Location -> (Board,Maybe Piece)
pickUpPiece (Board m cra ept) l = (Board (Map.update removePiece l m) cra ept, p')
  where
    p = fromRight (pieceAt l (Board m cra ept))
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
lineOfSightEndingInUnmovedTeamRook b (Team aff t) l direction
  | p == Right Nothing = if null restOfSquares then [] else targetLoc : restOfSquares
  | isTeammateUnmovedRook p (Team aff t) = [targetLoc]
  | otherwise = []
  where
    targetLoc = relLoc l aff direction
    p = pieceAt targetLoc b
    restOfSquares = lineOfSightEndingInUnmovedTeamRook b (Team aff t) targetLoc direction

emptyOrEnemy :: Board -> Team -> [Location] -> [Location]
emptyOrEnemy _ _ [] = []
emptyOrEnemy b t (l:ls)
  | p == Right Nothing || isEnemy t p = l : emptyOrEnemy b t ls
  | otherwise = emptyOrEnemy b t ls
  where p = pieceAt l b

-- assumes the location you send in is a pawn, this will return the forward left/right positions
-- if en passant is possible for either side.
getEnPassantTargetLocations :: Board -> Location -> Piece -> [Location]
getEnPassantTargetLocations (Board _ _ Nothing) _ _ = []
getEnPassantTargetLocations (Board _ _ (Just ept)) l (Piece (Team aff t) Pawn _) =
  [ x | x <- [fwdr l aff 1,fwdl l aff 1], x == ept ]

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
isKingInCheck t b deep =
  not (null kingSquares) -- HACK: a bit of a hack so that I don't have to put a king in all my tests
    && isLocationCapturable t b kingLoc deep
  where
    kingSquares = [ loc | (loc,Piece _ c _) <- mySquares t b, c == King ]
    kingLoc = head kingSquares

isLocationCapturable :: Team -> Board -> Location -> Int -> Bool
isLocationCapturable _ _ _ 0 = False
isLocationCapturable t b l deep = deep > 0 && l `elem` possibleEnemyLocs
  where
    enemyLocs = [ loc | (loc,_) <- enemySquaresThatCouldAttack t b l ]
    possibleEnemyLocs = [ loc | Move (_,loc) _ <- concatMap (\l' -> fromRight (possibleMovesFromLocation b l' (deep - 1))) enemyLocs ]

-- Added for the castling check to make sure nothing is in check
isLocationImmediatelyThreatened :: Team -> Board -> Location -> Bool
isLocationImmediatelyThreatened t b l = l `elem` possibleEnemyLocs
  where
    enemyLocs = [ loc | (loc,_) <- enemySquaresThatCouldAttack t b l ]
    possibleEnemyLocs = [ loc | Move (_,loc) _ <- concatMap (\l' -> fromRight (possibleMovesFromLocation b l' 0)) enemyLocs ]

mySquares :: Team -> Board -> [(Location,Piece)]
mySquares t (Board m _ _) =
  [ (loc,fromJust square) | (loc,square) <- Map.toList m,
    isJust square,
    getTeam (fromJust square) == t ]

myPossibleMoves :: Team -> Board -> [Move]
myPossibleMoves t b = concatMap (\l' -> fromRight (possibleMovesFromLocation b l' 1)) myLocs
  where
    myLocs = map fst (mySquares t b)

enemySquares :: Team -> Board -> [(Location,Piece)]
enemySquares t (Board m _ _) =
  [ (loc,fromJust square) | (loc,square) <- Map.toList m,
    isJust square,
    getTeam (fromJust square) /= t ]

enemySquaresThatCouldAttack :: Team -> Board -> Location -> [(Location,Piece)]
enemySquaresThatCouldAttack (Team aff tname) (Board m cra ept) l =
  [ (loc,fromJust square) | (loc,square) <- Map.toList m,
    isJust square,
    getTeam (fromJust square) /= t,
    loc `elem` allAttackableLocations ]
  where
    t = Team aff tname
    los = lineOfSightMaybeCapture (Board m cra ept) t l
    allAttackableLocations =
      los (0,1)
      ++ los (1,1)
      ++ los (1,0)
      ++ los (1,-1)
      ++ los (0,-1)
      ++ los (-1,-1)
      ++ los (-1,0)
      ++ los (-1,1)
      ++ [
        relLoc l aff (1,2),
        relLoc l aff (2,1),
        relLoc l aff (2,-1),
        relLoc l aff (1,-2),
        relLoc l aff (-1,-2),
        relLoc l aff (-2,-1),
        relLoc l aff (-2,1),
        relLoc l aff (-1,2)
      ]

