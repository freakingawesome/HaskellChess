module Hchess.Fen where

import Hchess.Board
import Hchess.Game
import Data.List (intercalate,sortBy)
import Data.Either.Unwrap
import Data.Char (toLower)
import Data.Maybe
import qualified Data.HashSet as HS
import Debug.Trace

toFen :: Game -> String
toFen (Game b (t@(Team _ tn):ts)) = intercalate "/" rows
  ++ " "
  ++ toLower (head tn) : ""
  ++ " "
  ++ toCastlingRookAvailability b
  where
    rows = map (toFenRow b) [7,6..0]

fromFen :: String -> Either String Game
fromFen = undefined

toFenRow :: Board -> Int -> String
toFenRow b@(Board sq _ _) y =
  concat $ map stringify $ foldr foldEmpties [] fenChars
  where
    fenChars = map (\x -> toFenChar (fromRight (pieceAt (x,y) b))) [0..7]
    foldEmpties (Left n) [] = Left n : []
    foldEmpties (Left n) (Left o:s) = Left (n + o) : s
    foldEmpties x xs = x : xs
    stringify (Left l) = show l
    stringify (Right r) = r : ""

toFenChar :: Maybe Piece -> Either Int Char
toFenChar Nothing = Left 1

toFenChar (Just p@(Piece (Team North "White") c)) = case c of
  Pawn -> Right 'P'
  Rook -> Right 'R'
  Knight -> Right 'N'
  Bishop -> Right 'B'
  Queen -> Right 'Q'
  King -> Right 'K'
  otherwise -> pieceNotHandled p

toFenChar (Just p@(Piece (Team South "Black") c)) = case c of
  Pawn -> Right 'p'
  Rook -> Right 'r'
  Knight -> Right 'n'
  Bishop -> Right 'b'
  Queen -> Right 'q'
  King -> Right 'k'
  otherwise -> pieceNotHandled p

toFenChar p = pieceNotHandled p
pieceNotHandled p = error ("Piece not handled: " ++ show p)

toCastlingRookAvailability :: Board -> String
toCastlingRookAvailability (Board map cra _) = fmap f (sortBy bottomLeftToTopRight $ HS.toList cra)
  where
    f loc = case loc of
      (0,0) -> 'K'
      (7,0) -> 'Q'
      (0,7) -> 'k'
      (7,7) -> 'q'
      otherwise -> error "Invalid rook starting location"
    bottomLeftToTopRight (ax,ay) (bx,by) = compare (ay,ax) (by,bx)

