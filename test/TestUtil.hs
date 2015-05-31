module TestUtil where

import SpecHelper
import qualified Data.Map as Map
import Data.Maybe(fromJust)
import Data.List(sort)
import Data.Either.Unwrap

boardSize :: Board -> Int
boardSize b = Map.size (squares b)

standardBoardPieceAt :: String -> Either String Square
standardBoardPieceAt s = pieceAt (fromAlgebraicLocation s) (newStandardBoard white black)

b8x8 :: [(Team,String)] -> Board
b8x8 ps = newBoard 8 8 ps

stdPossibleMoves :: [(Team,String)] -> String -> Either String [Move]
stdPossibleMoves ts al = stdPossibleMovesFromBoard (b8x8 ts) al

stdPossibleMovesFromBoard :: Board -> String -> Either String [Move]
stdPossibleMovesFromBoard b al = possibleMovesFromLocation b (fromAlgebraicLocation al) 1

getBoard :: Move -> Board
getBoard (Move _ b) = b

pieceCount :: Board -> Int
pieceCount b = Map.size (Map.filter (\x -> x /= Nothing) (squares b))

getTargetLocationsFromMoves :: Either String [Move] -> [Location]
getTargetLocationsFromMoves (Right []) = []
getTargetLocationsFromMoves (Left _) = []
getTargetLocationsFromMoves (Right (Move (_,to) _:ls)) = to : getTargetLocationsFromMoves (Right ls)

getBoardFromPossibleMoves :: Either String [Move] -> (Location,Location) -> Board
getBoardFromPossibleMoves pm ls = getBoard (head (filter (\(Move m _) -> m == ls) (fromRight pm)))

moveTargets :: Either String [Move] -> [String]
moveTargets (Right []) = []
moveTargets (Right (Move (_,to) _:ms)) = sort (toAlgebraicLocation to : moveTargets (Right ms))
moveTargets (Left _) = []

loc :: String -> Location
loc = fromAlgebraicLocation

locs :: [String] -> [Location]
locs [] = []
locs (x:xs) = loc x : locs xs

algloc :: Location -> String
algloc = toAlgebraicLocation

alglocs :: [Location] -> [String]
alglocs [] = []
alglocs (x:xs) = algloc x : alglocs xs


